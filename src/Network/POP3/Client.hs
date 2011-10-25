-- |
-- Module      :  Network.POP3.Client
-- Copyright   :  (c) 2009 Peter van den Brand
-- License     :  BSD3
--
-- Maintainer  :  peter@vdbrand.nl
-- Stability   :  provisional
-- Portability :  portable
--
-- This module contains function to connect to a POP3 server and 
-- retrieve messages and other information from it. 
--
-- This library is designed to be safe to use: connections are
-- guaranteed to be closed after the POP3 commands have been executed.
--
-- Example of downloading the latest email message:
--
-- @
--module Main where 
--
--import Network.POP3.Client
--
--main :: IO ()
--main = do
--    let account = POP3Account \"pop3.example.org\" defaultPort \"my_username\" \"my_password\"
--    messages <- withPOP3 account $ do
--        message <- withPOP3 account $ do
--        total <- getNumberOfMessages
--        getMessage total
--    putStrLn $ show message
-- @
--
-- Example using the hsemail package to parse the message headers and body:
--
-- @
--module Main where 
--
--import Network.POP3.Client
--import Control.Monad.Error
--import Text.ParserCombinators.Parsec
--import Text.ParserCombinators.Parsec.Error
--import qualified Text.ParserCombinators.Parsec.Rfc2822 as MP
--
--parseMessage s = case parse MP.message \"\" s of
--    Left err -> throwError $ concatMap messageString (errorMessages err)
--    Right m  -> return m
--
--main :: IO ()
--main = do
--    -- retrieve 5 latest messages and parse them using hsemail
--    let account = POP3Account \"pop3.example.org\" defaultPort \"my_username\" \"my_password\"
--    messages <- withPOP3 account $ do
--        total <- getNumberOfMessages
--        messages <- mapM getMessage $ take 5 (reverse [1..total])
--        mapM parseMessage messages
--    putStrLn $ show messages
-- @

module Network.POP3.Client (
        -- * Types
        POP3,
        POP3Account(..),
        MessageID,

        -- * Constants
        defaultPort,

        -- * Connecting and authenticating
        withPOP3,

        -- * Retrieving mailbox size
        getNumberOfMessages,
        getMailboxBytes,

        -- * Retrieving messages
        getUniqueID,
        getSize,
        getMessage,
        getFirstNLines,
        getHeaders

    ) where

import Network
import System.IO
import Data.List
import Data.Char
import Control.Exception
import Control.Monad.Error
import Control.Monad.Reader
import Control.Monad.Instances

-- | A record which contains all settings needed to connect to a POP3 server and to authenticate.
data POP3Account = POP3Account
    { accHostname :: String             -- ^ The hostname of the server to connect to
    , accPort     :: Int                -- ^ The port to connect to (use defaultPort if you want to use the default POP3 port)
    , accUsername :: String             -- ^ The username to login with
    , accPassword :: String             -- ^ The password to login with
    } deriving (Eq, Ord, Show)

-- | The socket handle (internal)
data Connection = Connection { socket :: Handle }

-- | The POP3 action. 
--   Encapsulates the network connection handle and provides error handling.
type POP3 = ErrorT String (ReaderT Connection IO)

-- | The message ID as the position in the list of messages on the server, from [1..getNumberOfMessages].
--   Note that this type does NOT represent the unique IDs (UIDL) of messages as returned by getUniqueID.
type MessageID = Integer

-- | Default POP3 port (110)
defaultPort :: Int
defaultPort = 110

-- | Connects to the given host and port, executes the given
--   POP3 action(s), closes the connection, and finally returns
--   the result op the (last) POP3 action.
--
--   If an error occurs, the action is aborted and an error message is returned.
--
--   The connection is guaranteed to be closed before returning from 
--   this function, even when an exception occurs during the session.
withPOP3 :: POP3Account -> POP3 a -> IO (Either String a)
withPOP3 account commands = withSocketsDo $ bracket connect disconnect session
    where
        connect    = do
            h <- connectTo (accHostname account) (PortNumber (fromIntegral (accPort account)))
            hSetBuffering h LineBuffering
            return $ Connection h
        disconnect = hClose . socket
        session    = runReaderT (runErrorT (receive singleLine >> authenticate account >> commands >>= quit))

-- | Send the given username and password. 
--   This has to be the first command sent to the POP3 server.
--   Other POP3 actions can only be executed after a successful authentication.
authenticate :: POP3Account -> POP3 String
authenticate account = do
    sendReceive ["USER", sanitize $ accUsername account] singleLine Right
    sendReceive ["PASS", sanitize $ accPassword account] singleLine Right

-- | Returns the number of messages stored in the POP3 mailbox.
getNumberOfMessages :: POP3 Integer
getNumberOfMessages = sendReceive ["STAT"] singleLine (firstToken toInt)

-- | Returns the size of the POP3 mailbox in bytes.
getMailboxBytes :: POP3 Integer
getMailboxBytes = sendReceive ["STAT"] singleLine (secondToken toInt)

-- | Returns the unique ID (UIDL) of a message on the server.
--   The message ID should be in the range [1..'getNumberOfMessages'].
getUniqueID :: MessageID -> POP3 String
getUniqueID n = sendReceive ["UIDL", show n] singleLine (secondToken Just)

-- | Returns the size of a message on the server in bytes.
--   Note that this may not correspond exactly to the size of the message
--   as it is downloaded, because of newline and escape values.
--   The message ID should be in the range [1..'getNumberOfMessages'].
getSize :: MessageID -> POP3 Integer
getSize n = sendReceive ["LIST", show n] singleLine (secondToken toInt)

-- | Retrieves a POP3 message from the server and returns it parsed as a 'Message'.
--   The message ID should be in the range [1..'getNumberOfMessages'].
getMessage :: MessageID -> POP3 String
getMessage n = sendReceive ["RETR", show n] multiLine Right

-- | Retrieves a the headers and the first n lines of a message from the server 
--   and returns it parsed as a 'Message'.
--   The message ID should be in the range [1..'getNumberOfMessages'].
getFirstNLines :: MessageID -> Integer -> POP3 String
getFirstNLines n m = sendReceive ["TOP", show n, show m] multiLine Right

-- | Retrieves a the headers of a message from the server and returns it parsed as a 'Message'.
--   The message ID should be in the range [1..'getNumberOfMessages'].
getHeaders :: MessageID -> POP3 String
getHeaders n = getFirstNLines n 0

-- | Sends the QUIT command to the server. It returns its argument to 
--   make the implementation of 'withPOP3' a little more concise.
quit :: a -> POP3 a
quit a = sendReceive ["QUIT"] singleLine Right >> return a

-------------------------------------------------------------------------------
-- Actual send and receive functions

sendReceive :: [String]             -- ^ The command and its arguments to send
    -> (Handle -> IO String)        -- ^ Function to read the response with
    -> (String -> Either String a)  -- ^ Function to parse the response with
    -> POP3 a                       -- ^ The parsed response
sendReceive command reader parser = do
    h <- asks socket
    liftIO $ hPutStr h (intercalate " " command)
    liftIO $ hPutStr h "\r\n"
    response <- receive reader
    case parser response of
        Left err -> throwError err
        Right r  -> return r

receive :: (Handle -> IO String)    -- ^ Function to read the response with
    -> POP3 String                  -- ^ The raw response body
receive reader = do
    h <- asks socket
    response <- liftIO $ reader h
    if "+OK" `isPrefixOf` response
        then return $ drop 4 response
        else throwError $ drop 5 (sanitize response)

-------------------------------------------------------------------------------
-- Small helper functions

-- remove all non-printable and all whitespace characters
sanitize :: String -> String
sanitize = filter (\c -> isPrint c && not (isSpace c))

firstToken, secondToken :: (String -> Maybe a) -> String -> Either String a
firstToken  = extractToken 0
secondToken = extractToken 1

-- extract the n'th word from a string and return the result of f applied to this word
extractToken :: Int -> (String -> Maybe a) -> String -> Either String a
extractToken n f input =
    case drop n (words input) of
        []    -> Left  $ "invalid response received: " ++ input
        (x:_) -> case f x of
                    Nothing -> Left  $ "invalid response received: " ++ input
                    Just x' -> Right x'

-- parse an integer from a string, returning Nothing if the 
-- string is empty or contains a non-digit character
toInt :: String -> Maybe Integer
toInt [] = Nothing
toInt s = helper s 0
    where
        helper :: String -> Integer -> Maybe Integer
        helper [] acc = Just acc
        helper (x:xs) acc
            | '0' <= x && x <= '9' = helper xs (acc * 10 + fromIntegral (ord x - ord '0'))
            | otherwise = Nothing

-- Read a single line from the POP3 connection.
-- According to the RFC, these lines should be terminated with CRLF.
singleLine :: Handle -> IO String
singleLine h = do
    line <- hGetLine h -- TODO should properly read upto CRLF instead of just LF, and discard the CRLF
    if "\r" `isSuffixOf` line 
        then return $ init line
        else return $ line

-- Read a multi-line response from the POP3 connection.
multiLine :: Handle -> IO String
multiLine h = do 
        firstLine <- singleLine h
        if not ("+OK" `isPrefixOf` firstLine)
            then return firstLine
            else do
                rest <- readOtherLines
                return $ "+OK " ++ joinWithCRLF (map removeTerminationOctet rest)
    where
        readOtherLines = do
            line <- singleLine h
            if line == "." 
                then return []
                else do
                    others <- readOtherLines
                    return $ line : others
        removeTerminationOctet s = if "." `isPrefixOf` s then tail s else s
        joinWithCRLF = concatMap (++ "\r\n")
