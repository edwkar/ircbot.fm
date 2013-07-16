{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}


import Control.Applicative              ((<$>))
import Control.Conditional              (ifM)
import Control.Monad                    (forever, void)
import Control.Monad.IO.Class           (MonadIO, liftIO)
import Control.Monad.Reader             (ReaderT, runReaderT, ask)
import Control.Monad.State              (StateT, runStateT, get, modify)
import Data.ByteString                  (ByteString)
import Data.ByteString.Char8            (pack)
import Data.ByteString.UTF8             (toString)
import Data.Char                        (isDigit)
import Data.Map                         (Map)
import Network.HTTP                     (simpleHTTP, getRequest,
                                         getResponseBody)
import System.Directory                 (doesFileExist)
import System.Environment               (getArgs)
import System.Exit                      (exitFailure)
import System.IO                        (Handle, BufferMode(LineBuffering),
                                         hSetBuffering, hPutStrLn, hGetLine,
                                         stderr)

import qualified Data.Map               as M
import qualified Network                as N
import qualified Network.IRC.Base       as IB
import qualified Network.IRC.Commands   as IC
import qualified Network.IRC.Parser     as IP
import qualified Text.Regex.PCRE.Light  as RE




type App = ReaderT BotConf (StateT BotState IO)

main :: IO ()
main = N.withSocketsDo $ do
    conf       <- confFromArgs

    handle     <- N.connectTo (serverHost conf) (serverPort conf)
    hSetBuffering handle LineBuffering

    startState <- mkStartState conf handle
    void $ runStateT (runReaderT runBot conf) startState

confFromArgs :: IO BotConf
confFromArgs = getArgs >>= build
  where
    build [bn, bc, sh, sp, amf, apiKey] | all isDigit sp =
      return BotConf
         { botNick        = bn
         , botChannel     = bc
         , serverHost     = sh
         , serverPort     = N.PortNumber $ fromIntegral (read sp :: Integer)
         , accountMapFile = amf
         , lastfmApiKey   = apiKey
         }

    build _ =
      do printError (
           "args: <bot nick> <channel> <server host> <server port> " ++
                 "<account mapping file> <last.fm api key>")
         exitFailure

mkStartState :: BotConf -> Handle -> IO BotState
mkStartState conf h = do m <- getMap
                         return $ BotState h m Nothing
  where
    getMap = ifM (doesFileExist $ accountMapFile conf)
                 readEntriesFromDisk
                 (return M.empty)

    readEntriesFromDisk = do
      rawEntries  <- lines <$> readFile (accountMapFile conf)
      let entries =  M.fromList [(a, b) | [a, b] <- map words rawEntries]
      return entries


--     --
-- IRC --
--     --

runBot :: App ()
runBot = do registerBot
            joinChannel
            forever act

registerBot :: App ()
registerBot = do nick    <- getBotNick
                 sendMsg $  IC.nick nick
                 sendMsg $  IC.user nick "x" "y" nick

joinChannel :: App ()
joinChannel = getBotChannel >>= sendMsg . IC.joinChan

act :: App ()
act = do m <- readMsg
         withMaybe m
                   actOnMessage
                   "could not understand message from server"

readMsg :: App (Maybe IB.Message)
readMsg = do h <- getBotHandle
             liftIO $ IP.decode <$> hGetLine h

actOnMessage :: IB.Message -> App ()
actOnMessage m = maybe (return ())
                       (\h -> h m)
                       (M.lookup (IB.msg_command m) handlers)

handlers :: Map IB.Command MessageHandler
handlers = M.fromList [("PING",    onPing   )
                      ,("PRIVMSG", onPrivMsg)
                      ]

onPing :: MessageHandler
onPing _ = sendMsg $ IB.Message Nothing "PONG" []

onPrivMsg :: MessageHandler
onPrivMsg (IB.Message (Just (IB.NickName ircNick _ _)) _ params) =
  do setCurRespondent ircNick (head params)
     case words $ params !! 1 of
       ["!fm", "register", lastfmName] ->
         fmRegister ircNick lastfmName

       ["!fm"] ->
         fmLookup ircNick

       _ -> return ()
onPrivMsg _ = error "unrecognized message format"

sendMsg :: IB.Message -> App ()
sendMsg m = do h      <- getBotHandle
               liftIO $  hPutStrLn h $ IB.encode m

respond :: String -> App ()
respond msg = do mcr <- getCurRespondent
                 withMaybe mcr
                           (\r -> sendMsg $ IC.privmsg r msg)
                           "no current respondent registered"


--         --
-- LAST.FM --
--         --

fmRegister :: IB.UserName -> LastFMUserName -> App ()
fmRegister ircNick lastfmName =
  if not (isValidFmAccountName lastfmName) then
    printError "invalid account name."
  else do
    addRegistration ircNick lastfmName
    respond $ "registered last.fm account " ++ lastfmName ++
                 " for user " ++ ircNick ++ "."
    saveMappingFile

addRegistration :: IB.UserName -> LastFMUserName -> App ()
addRegistration ircNick lastfmName = do
    numRegs    <- M.size <$> getAccountMap
    if numRegs >  maxNumRegistrations
    then
      liftIO $ do printError "too many registrations. chickening out!"
                  exitFailure
    else
      modifyNick2LastFmMap $ \curMap -> M.insert ircNick lastfmName curMap

saveMappingFile :: App ()
saveMappingFile = do amap           <- getAccountMap
                     fn             <- getAccountMapFile
                     let serialized = concatMap (\(a, b) -> a++" "++b++"\n")
                                                (M.toList amap)
                     liftIO $ writeFile fn serialized

fmLookup :: IB.UserName -> App ()
fmLookup ircNick =
  do mLastfmName <- M.lookup ircNick <$> getAccountMap
     case mLastfmName of
       Nothing ->
         respond $ "no account registered for " ++ ircNick ++ ". " ++
                        "(register with \"!fm register <account-name>\".)"

       Just lastfmName -> do
           url       <- fmUrlForAccount lastfmName
           body      <- pack <$> fetchUrl url
           withMaybe (fmParse body)
                     reportTrack
                     ("failed to fetch current track for " ++ lastfmName)
         where
           reportTrack (an, tn) = respond $ "%fm: " ++ an++" - "++tn

fmUrlForAccount :: LastFMUserName -> App String
fmUrlForAccount lastfmName = do
  apiKey <- getApiKey
  return $ concat ["http://ws.audioscrobbler.com/2.0/"
                  ,"?method=user.getrecenttracks"
                  ,"&user=", lastfmName
                  ,"&api_key=", apiKey
                  ,"&format=json"
                  ,"&limit=1"
                  ]

fmParse :: ByteString -> Maybe (String, String)
fmParse body =
    case RE.match pat body [] of
      Just [_, artistName, trackName] ->
        Just (toString artistName, toString trackName)
      Nothing ->
        Nothing
      Just _ ->
        error "unrecognized match result"
  where
    pat = RE.compile "^.*?#text\":\"([^\"]+).*?name\":\"([^\"]+).*$" []

maxNumRegistrations :: Int
maxNumRegistrations = 4242

isValidFmAccountName :: LastFMUserName -> Bool
isValidFmAccountName an = an /= "" -- TODO!


--       --
-- TYPES --
--       --

type MessageHandler = IB.Message -> App ()

type LastFMUserName = String
type LastFMAPIKey   = String
type ChannelName    = String
type AccountMap     = Map IB.UserName LastFMUserName

data BotState = BotState { botHandle       :: Handle
                         , nick2LastFMMap  :: AccountMap
                         , curRespondent   :: Maybe IB.UserName
                         }
getBotHandle             :: App Handle
getBotHandle             = botHandle <$> get
getCurRespondent         :: App (Maybe IB.UserName)
getCurRespondent         = curRespondent <$> get
getAccountMap            :: App AccountMap
getAccountMap            = nick2LastFMMap <$> get

setCurRespondent :: String -> String -> App ()
setCurRespondent nick target = modify $ \s ->
  s { curRespondent = Just $ if head target == '#'
                             then target
                             else nick }

modifyNick2LastFmMap :: (AccountMap -> AccountMap) -> App ()
modifyNick2LastFmMap f = do m      <- getAccountMap
                            modify $ \s -> s { nick2LastFMMap = f m }

data BotConf = BotConf { botNick        :: IB.UserName
                       , botChannel     :: ChannelName
                       , serverHost     :: N.HostName
                       , serverPort     :: N.PortID
                       , accountMapFile :: FilePath
                       , lastfmApiKey   :: LastFMAPIKey
                       }
getBotNick             :: App IB.UserName
getBotNick             = botNick        <$> ask
getBotChannel          :: App ChannelName
getBotChannel          = botChannel     <$> ask
getAccountMapFile      :: App FilePath
getAccountMapFile      = accountMapFile <$> ask
getApiKey              :: App LastFMAPIKey
getApiKey              = lastfmApiKey   <$> ask


--       --
-- UTILS --
--       --

printError :: MonadIO m => String -> m ()
printError = liftIO . hPutStrLn stderr

withMaybe :: Maybe a -> (a -> App ()) -> String -> App ()
withMaybe x a errStr = maybe (printError errStr) a x

fetchUrl :: MonadIO m => String -> m String
fetchUrl url = liftIO $ simpleHTTP (getRequest url) >>=
                          fmap (take 424242) . getResponseBody
