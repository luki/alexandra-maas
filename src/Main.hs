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

nice = True
sorry = False

sendTLSMail :: SMTP_Addr
            -> Settings
            -> Username
            -> Password
            -> Receiver
            -> Subject
            -> Message
            -> IO ()
sendTLSMail addr settings name pw receiver subject msg =
  doSMTPSTARTTLSWithSettings addr settings $ \con -> do
        status <- authenticate LOGIN name pw con
        case status of
            nice  -> sendPlainTextMail receiver name subject (L.pack msg) con
            sorry -> putStrLn "Couldn't connect."

main :: IO ()
main = do
    let settings = Settings 587 500 True True
        handle   = "lukemsworld@gmail.com"
        pw       = ""
        addr     = "smtp.gmail.com"
        receiver = "lukemueller@protonmail.com"
        sub      = "sorry 4 spam"
        msg      = "some text (:"

    sendTLSMail addr settings handle pw receiver sub msg
    putStrLn "Test"
