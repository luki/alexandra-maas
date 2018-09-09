module Main where

import qualified Data.Text.Lazy     as L
import qualified Data.Text          as T
import           Data.Text.Encoding           (decodeUtf8)
import qualified Data.Text.IO       as TextIo

import Network.HaskellNet.SMTP.SSL
import Network.HaskellNet.SMTP     (authenticate, sendPlainTextMail)
import Network.HaskellNet.Auth     (AuthType(LOGIN, PLAIN))

import           Network.HaskellNet.IMAP.SSL         (connectIMAPSSL)
import qualified Network.HaskellNet.IMAP     as IMAP

import Data.ByteString (ByteString)
import Control.Monad   (when)

type SMTP_Addr = String
type Username = String
type Password = String
type Receiver = String
type Subject = String
type Message = String

sendTLSMail :: SMTP_Addr
            -> Settings
            -> Username
            -> Password
            -> Receiver
            -> Subject
            -> Message
            -> IO ()
sendTLSMail addr settings name pw receiver subject msg =
  doSMTPSTARTTLSWithSettings addr settings $
      \con -> do status <- authenticate LOGIN name pw con
                 case status of
                     True  -> sendPlainTextMail receiver name subject (L.pack msg) con
                     False -> putStrLn "Couldn't connect."

ioByteStringsToText :: [IO ByteString] -> IO [T.Text]
ioByteStringsToText [x] = do
    text <- x
    return [decodeUtf8 text]
ioByteStringsToText (x:xs) = do
    textList <- ioByteStringsToText xs
    text <- x
    return [decodeUtf8 text]

-- printList :: (Show a, Eq a) => [a] -> IO ()
-- printList (x:xs) = do
--     putStrLn . show $ x
--     when (xs /= []) $ (printList xs)

-- Try to generalize (without bytestring)
printList :: [IO ByteString] -> IO ()
printList (x:xs) = do
    putStrLn . show . decodeUtf8 =<< x
    when (null xs) $ printList xs

main :: IO ()
main = do
    let settings = Settings 587 500 True True
        handle   = ""
        pw       = ""
        addr     = ""
        receiver = ""
        sub      = "sorry 4 spam"
        msg      = "some text (:"

    imapCon <- connectIMAPSSL ""
    IMAP.authenticate imapCon IMAP.PLAIN "" ""
    IMAP.select imapCon "Inbox"
    uids <- IMAP.search imapCon [IMAP.NOTs $ IMAP.FLAG IMAP.Seen]

    let ioByteStrings = map (IMAP.fetch imapCon) uids -- [IO ByteString]
    printList ioByteStrings

    putStrLn . show $ uids

    -- sendTLSMail addr settings handle pw receiver sub msg
    putStrLn "Test"
