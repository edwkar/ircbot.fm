{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

import Prelude hiding                   (catch)

import Control.Applicative              ((<$>), (<*>), (<*))
import Control.Concurrent               (forkIO)
import Control.Exception                (IOException, catch, evaluate)
import Control.Monad                    (MonadPlus, mzero, forever, void,
                                         when, unless, liftM)
import Data.Aeson                       (FromJSON(..), decode, (.:))
import Data.Aeson.Types                 (parseMaybe, Object)
import Data.ByteString.Lazy             (ByteString)
import Data.ByteString.Lazy.Char8       (pack)
import Control.Monad.Error              (ErrorT, runErrorT, throwError)
import Control.Monad.IO.Class           (MonadIO, liftIO)
import Control.Monad.Reader             (ReaderT, runReaderT, ask)
import Control.Monad.State              (StateT, runStateT, get, put, modify)
import Data.List                        (isInfixOf, sort)
import Data.Map                         (Map)
import Data.Text                        ()
import Data.Maybe                       (maybe)
import Network.HTTP                     (simpleHTTP, getRequest,
                                         getResponseBody)
import System.Environment               (getArgs)
import System.Exit                      (ExitCode(ExitFailure), exitWith)
import System.IO                        (Handle, BufferMode(NoBuffering),
                                         hSetBuffering, hPutStr, hPutStrLn,
                                         hClose, hGetLine, withFile,
                                         hGetContents, IOMode(ReadMode),
                                         stderr)

import qualified Data.ByteString.Char8  as B
import qualified Data.Map               as M
import qualified Data.Text              as T
import qualified Network                as N
import qualified Network.IRC.Base       as IB
import qualified Network.IRC.Commands   as IC
import qualified Network.IRC.Parser     as IP
import qualified System.Directory       as D


type LastFMUserName = String



data BotState = BotState {
                  botHandle       :: Handle,
                  nick2LastFMMap  :: Map IB.UserName LastFMUserName,
                  curRespondent   :: Maybe String
                }

data BotConfig = BotConfig {
                   botServer :: N.HostName
                 , botPort   :: N.PortID
                 , botNick   :: IB.UserName
                 }


type App = ReaderT BotConfig (StateT BotState IO)



main :: IO ()
main = N.withSocketsDo $ do
    args <- getArgs

    let conf = BotConfig "irc.inet.tele.dk" (N.PortNumber 6667) "loldog"
    handle <- N.connectTo (botServer conf) (botPort conf)
    hSetBuffering handle NoBuffering

    let botState = BotState handle M.empty Nothing
    void $ runStateT (runReaderT runBot conf) botState

runBot :: App ()
runBot = registerBot >> joinChannels >> runMainLoop

registerBot :: App ()
registerBot = do
  nick    <- getBotNick
  ircSend $ IC.nick nick
  ircSend $ IC.user nick "x" "y" nick

joinChannels :: App ()
joinChannels = ircSend $ IC.joinChan "#wtflulz"

runMainLoop :: App ()
runMainLoop = void $ forever act

act :: App ()
act = readMsg >>= \m -> withMaybe m
                                  actOnMessage
                                  "could not understand message from server"

readMsg :: App (Maybe IB.Message)
readMsg = do
  h <- getBotHandle
  liftIO $ IP.decode <$> hGetLine h

actOnMessage :: IB.Message -> App ()
actOnMessage m = do
  whenDebugLog $ "acting on " ++ show m
  withMaybe (M.lookup (IB.msg_command m) handlers)
            (\h -> h m)
            ("no handler for " ++ show m)


type CommandHandler = IB.Message -> App ()

handlers :: Map IB.Command CommandHandler
handlers = M.fromList [("PING",    onPing  )
                      ,("PRIVMSG", onPrivMsg)
                      ]

onPing :: CommandHandler
onPing _ = ircSend $ IB.Message Nothing "PONG" []

onPrivMsg :: CommandHandler
onPrivMsg (IB.Message (Just (IB.NickName ircNick _ _)) _ params) = do
  setCurRespondent ircNick (head params)
  case words $ params !! 1 of
    ["!fm", "register", lastfmName] ->
      fmRegister ircNick lastfmName

    ["!fm"] ->
      fmLookup ircNick

    otherwise -> liftIO $ print params

fmRegister :: IB.UserName -> LastFMUserName -> App ()
fmRegister ircNick lastfmName =
  if not (isValidAccountName lastfmName) then
    printError "invalid account name."
  else do
    addRegistration ircNick lastfmName
    ircRespond $ "registered last.fm account " ++ lastfmName ++
                 " for user " ++ ircNick ++ "."

addRegistration :: IB.UserName -> LastFMUserName -> App ()
addRegistration ircNick lastfmName = do
  modify $ \s ->
    s { nick2LastFMMap = M.insert ircNick lastfmName (nick2LastFMMap s) }

setCurRespondent nick target =
  modify $ \s ->
    s { curRespondent = Just $ if head target == '#'
                               then target
                               else nick }

ircRespond msg =
  get >>= \s -> withMaybe (curRespondent s)
                (\r -> ircSend $ IC.privmsg r msg)
                "no current respondent registered"

fmLookup :: IB.UserName -> App ()
fmLookup ircNick = do
    mLastfmName <- M.lookup ircNick <$> nick2LastFMMap <$> get
    case mLastfmName of
      Nothing -> do
        ircRespond $ "no account registered for " ++ ircNick ++ ". " ++
                       "(register with \"!fm register <account-name>\".)"

      Just lastfmName -> do
          body <- pack <$> fetchUrl (urlForAccount lastfmName)
          liftIO $ print body
          liftIO $ print (decode body :: Maybe Object)
          withMaybe (fmParse body)
                    (\(artistName, trackName) ->
                      ircRespond $ "%fm: " ++ artistName ++
                                              " - " ++ trackName)
                    ("failed to fetch current track for " ++ lastfmName)


urlForAccount lastfmName = concat
  ["http://ws.audioscrobbler.com/2.0/?"
  ,"method=user.getrecenttracks&", "user=", lastfmName
  ,"&api_key=e8c9a75125859e44572ce65828f35a8e"
  ,"&format=json&limit=1"
  ]

fmParse :: ByteString -> Maybe (String, String)
fmParse body = decode body >>= \res ->
  flip parseMaybe res $ \obj -> do
    rt         :: Object <- obj    .: "recenttracks"
    track      :: Object <-   rt     .: "track"
    artist     :: Object <-     track  .: "artist"
    artistName :: String <-       artist .: "#text"
    trackName  :: String <-   track  .: "name"
    return (artistName, trackName)

fetchUrl url = liftIO $
  simpleHTTP (getRequest url) >>= fmap (take 424242) . getResponseBody

ircSend :: IB.Message -> App ()
ircSend m = do
  h            <- getBotHandle
  liftIO       $ hPutStrLn h $ IB.encode m
  whenDebugLog $ "sent " ++ (show m)

whenDebugLog = liftIO . hPutStrLn stderr

getBotHandle :: App Handle
getBotHandle = botHandle <$> get

getBotNick = botNick <$> ask

printError = liftIO . hPutStrLn stderr

withMaybe x a errStr =
  maybe (printError errStr) a x

isValidAccountName an = an /= "" -- TODO!
