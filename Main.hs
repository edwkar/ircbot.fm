{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

import Prelude hiding                   (catch)

import Control.Applicative              ((<$>), (<*>), (<*))
import Control.Conditional              (ifM)
import Control.Exception                (IOException, catch, evaluate)
import Control.Monad                    (MonadPlus, mzero, forever, void,
                                         when, unless, liftM)
import Control.Monad.Error              (ErrorT, runErrorT, throwError)
import Control.Monad.IO.Class           (MonadIO, liftIO)
import Control.Monad.Reader             (ReaderT, runReaderT, ask)
import Control.Monad.State              (StateT, runStateT, get, put, modify)
import Data.Aeson                       (FromJSON(..), decode, (.:))
import Data.Aeson.Types                 (parseMaybe, Object)
import Data.ByteString                  (ByteString)
import Data.ByteString.Char8            (pack)
import Data.ByteString.UTF8             (toString)
import Data.Char                        (isDigit)
import Data.List                        (isInfixOf, sort)
import Data.Map                         (Map)
import Data.Maybe                       (maybe)
import Data.Text                        ()
import Network.HTTP                     (simpleHTTP, getRequest,
                                         getResponseBody)
import System.Directory                 (doesFileExist)
import System.Environment               (getArgs)
import System.Exit                      (exitFailure)
import System.IO                        (Handle, BufferMode(LineBuffering),
                                         FilePath, hSetBuffering, hPutStr,
                                         hPutStrLn, hClose, hGetLine,
                                         withFile, writeFile, hGetContents,
                                         IOMode(ReadMode), stderr)

import qualified Data.ByteString
import qualified Data.Map               as M
import qualified Data.Text              as T
import qualified Network                as N
import qualified Network.IRC.Base       as IB
import qualified Network.IRC.Commands   as IC
import qualified Network.IRC.Parser     as IP
import qualified Text.Regex.PCRE.Light  as RE


type LastFMUserName = String
type LastFMAPIKey   = String
type ChannelName    = String



data BotState = BotState { botHandle       :: Handle
                         , nick2LastFMMap  :: Map IB.UserName LastFMUserName
                         , curRespondent   :: Maybe String
                         }

data BotConfig = BotConfig { botNick        :: IB.UserName
                           , botChannel     :: ChannelName
                           , serverHost     :: N.HostName
                           , serverPort     :: N.PortID
                           , accountMapFile :: FilePath
                           , lastfmApiKey   :: LastFMAPIKey
                           }


type App = ReaderT BotConfig (StateT BotState IO)

main :: IO ()
main = N.withSocketsDo $ do
    conf   <- confFromArgs

    handle <- N.connectTo (serverHost conf) (serverPort conf)
    hSetBuffering handle LineBuffering

    startState <- mkStartState conf handle
    void $ runStateT (runReaderT runBot conf) startState

confFromArgs :: IO BotConfig
confFromArgs = getArgs >>= build
  where
    build [bn, bc, sh, sp, amf, apiKey] | all isDigit sp =
      return BotConfig { botNick        = bn
                       , botChannel     = bc
                       , serverHost     = sh
                       , serverPort     = N.PortNumber $ fromIntegral $ read sp
                       , accountMapFile = amf
                       , lastfmApiKey   = apiKey
                       }
    build _ =
      do printError (
           "args: <bot nick> <channel> <server host> <server port> " ++
                 "<account mapping file> <last.fm api key>")
         exitFailure

mkStartState :: BotConfig -> Handle -> IO BotState
mkStartState conf h = do m      <- getMap
                         return $ BotState h m Nothing
  where
    getMap =
      ifM (doesFileExist $ accountMapFile conf)
          (do rawEntries <- lines <$> (readFile $ accountMapFile conf)
              let entries = M.fromList [(a, b) | [a, b] <- map words rawEntries]
              putStrLn $ "Read " ++ show (M.size entries) ++
                           " entries from disk"
              return entries)
          (return M.empty)

runBot :: App ()
runBot = do registerBot
            joinChannel
            forever act

registerBot :: App ()
registerBot = do nick    <- getBotNick
                 ircSend $ IC.nick nick
                 ircSend $ IC.user nick "x" "y" nick

joinChannel :: App ()
joinChannel = getBotChannel >>= ircSend . IC.joinChan

act :: App ()
act = readMsg >>= \m -> withMaybe m
                                  actOnMessage
                                  "could not understand message from server"

readMsg :: App (Maybe IB.Message)
readMsg = do h <- getBotHandle
             liftIO $ IP.decode <$> hGetLine h

actOnMessage :: IB.Message -> App ()
actOnMessage m = do whenDebugLog $ "acting on " ++ show m
                    maybe (return ())
                          (\h -> h m)
                          (M.lookup (IB.msg_command m) handlers)

type CommandHandler = IB.Message -> App ()

nullHandler m = return ()

handlers :: Map IB.Command CommandHandler
handlers = M.fromList [("PING",    onPing  )
                      ,("PRIVMSG", onPrivMsg)
                      ,("NOTICE",  nullHandler)
                      ]

onPing :: CommandHandler
onPing _ = ircSend $ IB.Message Nothing "PONG" []

onPrivMsg :: CommandHandler
onPrivMsg (IB.Message (Just (IB.NickName ircNick _ _)) _ params) =
  do setCurRespondent ircNick (head params)
     case words $ params !! 1 of
       ["!fm", "register", lastfmName] ->
         fmRegister ircNick lastfmName

       ["!fm"] ->
         fmLookup ircNick

       otherwise -> return ()

fmRegister :: IB.UserName -> LastFMUserName -> App ()
fmRegister ircNick lastfmName =
  if not (isValidAccountName lastfmName) then
    printError "invalid account name."
  else do
    addRegistration ircNick lastfmName
    ircRespond $ "registered last.fm account " ++ lastfmName ++
                 " for user " ++ ircNick ++ "."
    saveMappingFile

saveMappingFile :: App ()
saveMappingFile = do
  m <- nick2LastFMMap <$> get
  f <- getAccountMapFile
  let serialized = concatMap (\(a, b) -> a++" "++b++"\n") (M.toList m)
  liftIO $ writeFile f serialized

addRegistration :: IB.UserName -> LastFMUserName -> App ()
addRegistration ircNick lastfmName = do
  modify $ \s ->
    s { nick2LastFMMap = M.insert ircNick lastfmName (nick2LastFMMap s) }

setCurRespondent :: String -> String -> App ()
setCurRespondent nick target =
  modify $ \s ->
    s { curRespondent = Just $ if head target == '#'
                               then target
                               else nick }

ircRespond :: String -> App ()
ircRespond msg =
  get >>= \s -> withMaybe (curRespondent s)
                (\r -> ircSend $ IC.privmsg r msg)
                "no current respondent registered"

fmLookup :: IB.UserName -> App ()
fmLookup ircNick =
  do mLastfmName <- M.lookup ircNick <$> nick2LastFMMap <$> get
     case mLastfmName of
       Nothing -> do
         ircRespond $ "no account registered for " ++ ircNick ++ ". " ++
                        "(register with \"!fm register <account-name>\".)"

       Just lastfmName -> do
           url    <- urlForAccount lastfmName
           body   <- pack <$> fetchUrl url
           liftIO $ print body
           withMaybe (fmParse body)
                     (\(artistName, trackName) ->
                       ircRespond $ "%fm: " ++ artistName ++
                                               " - " ++ trackName)
                     ("failed to fetch current track for " ++ lastfmName)


urlForAccount :: LastFMUserName -> App String
urlForAccount lastfmName = do
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
  where
    pat = RE.compile "^.*?#text\":\"([^\"]+).*?name\":\"([^\"]+).*$" []

fetchUrl :: String -> App String
fetchUrl url = liftIO $
  simpleHTTP (getRequest url) >>= fmap (take 424242) . getResponseBody

ircSend :: IB.Message -> App ()
ircSend m = do h            <- getBotHandle
               liftIO       $ hPutStrLn h $ IB.encode m
               whenDebugLog $ "sent " ++ (show m)

whenDebugLog :: String -> App ()
whenDebugLog x = return () -- printError

getApiKey         = lastfmApiKey   <$> ask :: App LastFMAPIKey
getBotChannel     = botChannel     <$> ask :: App ChannelName
getBotHandle      = botHandle      <$> get :: App Handle
getBotNick        = botNick        <$> ask :: App IB.UserName
getAccountMapFile = accountMapFile <$> ask :: App FilePath

printError :: MonadIO m => String -> m ()
printError = liftIO . hPutStrLn stderr

withMaybe :: Maybe a -> (a -> App ()) -> String -> App ()
withMaybe x a errStr = maybe (printError errStr) a x

isValidAccountName :: String -> Bool
isValidAccountName an = an /= "" -- TODO!
