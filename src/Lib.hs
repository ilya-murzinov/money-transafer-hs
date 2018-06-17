{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators      #-}

module Lib
    ( runServer
    ) where

import           Control.Concurrent.STM     (STM, TVar, atomically, newTVar,
                                             orElse, readTVar, registerDelay,
                                             retry, writeTVar)
import           Control.Monad.IO.Class     (liftIO)
import           Control.Monad.Trans.Reader (ReaderT, ask, runReaderT)
import           Data.Aeson                 (FromJSON, ToJSON)
import           Data.ByteString.Lazy.Char8 (ByteString, intercalate, pack)
import qualified Data.Map                   as Map
import           Data.Monoid                ((<>))
import           Data.Traversable           (sequenceA)
import           GHC.Generics               (Generic)
import           Network.Wai.Handler.Warp   (run)
import           Servant

-- Types

newtype AccountId = AccountId Integer
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)
data Account = Account
  { id      :: AccountId
  , balance :: Integer
  } deriving (Show, Generic, ToJSON, FromJSON)
data Transfer = Transfer
  { fromAccount :: AccountId
  , toAccount   :: AccountId
  , amount      :: Integer
  } deriving (Show, Generic, ToJSON, FromJSON)
instance FromHttpApiData AccountId where
  parseUrlPiece p = do
    s <- parseUrlPiece p
    return $ AccountId s

data TransferResult = Success | Fail [Error]
data Error = AccountNotExists AccountId
  | InsufficientFunds AccountId
  | WrongAmount Integer
  | Other

type State = Map.Map AccountId (TVar Account)

newtype Env = Env (TVar State)

type AppM = ReaderT Env Handler

type AID = Capture "accountId" AccountId
type GetAccountEndpoint = AID :> Get '[JSON] Account
type CreateAccountEndpoint = PostCreated '[JSON] Account
type TransferEndpoint = ReqBody '[JSON] Transfer :> PostCreated '[JSON] Transfer

type API = "api" :>
  (    "accounts" :> (GetAccountEndpoint :<|> CreateAccountEndpoint)
  :<|> "transfer" :> TransferEndpoint
  )

-- Logic

transferMoney :: State -> AccountId -> AccountId -> Integer -> STM TransferResult
transferMoney state fromId toId amount =
  if amount < 0 then return $ Fail [WrongAmount amount]
  else do
    let
      fromAccM = Map.lookup fromId state
      toAccM = Map.lookup toId state
      changeBalance s tv amount = do
        acc <- readTVar tv
        _ <- writeTVar tv $ acc{balance = balance acc + amount}
        return ()
    case (fromAccM, toAccM) of
      (Nothing, Nothing)   -> return $ Fail [AccountNotExists fromId, AccountNotExists toId]
      (Nothing, Just _)    -> return $ Fail [AccountNotExists fromId]
      (Just _, Nothing)    -> return $ Fail [AccountNotExists toId]
      (Just fromTV, Just toTV) -> do
        Account _ fromAmount <- readTVar fromTV
        if fromAmount >= amount then do
          _ <- changeBalance state fromTV (- amount)
          _ <- changeBalance state toTV amount
          return Success
        else retry

-- Endpoints

getAccount :: AccountId -> AppM Account
getAccount id = do
  Env s <- ask
  acc <- liftIO $ atomically $ do
    state <- readTVar s
    sequenceA $ readTVar <$> Map.lookup id state
  case acc of
    Just a  -> return a
    Nothing -> throwError err404

createAccount :: AppM Account
createAccount = do
  Env s <- ask
  liftIO $ atomically $
    do
      state <- readTVar s
      let
        ids = (\(AccountId id) -> id) <$> Map.keys state
        id = AccountId $ case ids of
          [] -> 1
          l  -> maximum l + 1
        acc = Account id 0
      tv <- newTVar acc
      writeTVar s $ Map.insert id tv state
      return acc

transfer :: Transfer -> AppM Transfer
transfer t@(Transfer from to amount) = do
  Env s <- ask
  timer <- liftIO $ registerDelay 1000
  result <- liftIO $ atomically $ do
    state <- readTVar s
    let
      timeout = do
        b <- readTVar timer
        if b then
          return $ Fail [Other]
        else retry
    (transferMoney state from to amount) `orElse` timeout
  case result of
    Success   -> return t
    Fail errs -> throwError err400{errBody = intercalate "," $ showError <$> errs}

-- Helpers

showError :: Error -> ByteString
showError (AccountNotExists id) = pack $ "Account '" <> show id <> "' doesn't exist"
showError (InsufficientFunds id) = pack $ "Account '" <> show id <> "' has insufficient funds"
showError (WrongAmount amount) = pack $ "Amount '" <> show amount <> "' is incorrect"
showError Other = "Something went wrong"

-- Server

api :: Proxy API
api = Proxy

server :: ServerT API AppM
server = (getAccount :<|> createAccount) :<|> transfer

nt :: Env -> AppM a -> Handler a
nt s x = runReaderT x s

app :: Env -> Application
app s = serve api $ hoistServer api (nt s) server

runServer :: Integer -> IO ()
runServer port = do
  let masterAccId = AccountId 0
  masterAcc <- liftIO $ atomically $ newTVar $ Account masterAccId 1000
  state <- liftIO $ atomically $ newTVar $ Map.singleton masterAccId masterAcc
  run (fromInteger port) $ app $ Env state
