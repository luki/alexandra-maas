module Main where

import qualified Data.Text.Lazy as L
import qualified Data.Text      as T

import Network.HaskellNet.SMTP.SSL
import Network.HaskellNet.SMTP     (authenticate, sendPlainTextMail)
import Network.HaskellNet.Auth     (AuthType(LOGIN, PLAIN))

import qualified Data.Bson as B
import Data.Bson         (Field( (:=) ))
import Data.Bson.Mapping

import Control.Applicative (pure)

data Subscriber = Subscriber
     { firstName :: String
     , lastName :: String
     , mailAddress :: String
     } deriving (Show, Eq)

instance Bson Subscriber where
    fromBson bsonData =
        do firstName <- B.lookup (T.pack "firstName") bsonData
           lastName <- B.lookup (T.pack "lastName") bsonData
           addr <- B.lookup (T.pack "mailAddress") bsonData
           pure $ Subscriber firstName lastName addr
    toBson (Subscriber firstName lastName addr) =
        [(T.pack "firstName") := (B.String . T.pack $ firstName)
        ,(T.pack "lastName") := (B.String . T.pack $ lastName)
        ,(T.pack "mailAddress") := (B.String . T.pack $ addr)
        ]

data Subscription = Subscription
    { subscriber :: Subscriber
    , listId :: String
    } deriving (Eq, Show)

instance Bson Subscription where
    fromBson bsonData =
        do listId <- (B.lookup (T.pack "listId") bsonData)
           subscriber <- B.lookup (T.pack "subscriber") bsonData
           finalSub <- (fromBson subscriber)
           pure $ Subscription finalSub listId
    toBson (Subscription sub listId) =
        [(T.pack "subscriber") := (B.Doc $ toBson sub)
        ,(T.pack "listId") := (B.String . T.pack $ listId)
        ]

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
  doSMTPSTARTTLSWithSettings addr settings $ \con ->
        do status <- authenticate LOGIN name pw con
           case status of
                True  -> sendPlainTextMail receiver name subject (L.pack msg) con
                False -> putStrLn "Couldn't connect."

main :: IO ()
main = do
    -- Test BSON
    let sub = Subscription
                  (Subscriber "Luke" "Mueller" "lukasamueller@icloud.com")
                  "0"

    (putStrLn . show) =<< (fromBson (toBson sub) :: IO Subscription)

    let settings = Settings 587 500 True True
        handle   = "lukemsworld@gmail.com"
        pw       = ""
        addr     = "" --"smtp.gmail.com"
        receiver = "lukemueller@protonmail.com"
        sub      = "sorry 4 spam"
        msg      = "some text (:"

    sendTLSMail addr settings handle pw receiver sub msg
    putStrLn "Test"
