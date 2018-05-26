module Main where

import Network.HaskellNet.SMTP.SSL
import Network.HaskellNet.SMTP     (authenticate, sendPlainTextMail)
import Network.HaskellNet.Auth     (AuthType(LOGIN, PLAIN))

import qualified Data.Text.Lazy as L

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
sendTLSMail addr stngs name pw rcv sub msg =
  doSMTPSTARTTLSWithSettings addr stngs $ \con -> do
        status <- authenticate LOGIN name pw con
        case status of
            True  -> sendPlainTextMail rcv name sub (L.pack msg) con
            False -> putStrLn "Couldn't connect."

main :: IO ()
main = do
    let settings = Settings 587 500 True True
        handle   = "lukemsworld@gmail.com"
        pw       = ""
        addr     = "smtp.gmail.com"
        rcv      = "lukemueller@protonmail.com"
        sub      = "sorry 4 spam"
        msg      = "some text (:"

    sendTLSMail addr settings handle pw rcv sub msg
    putStrLn "Test"
