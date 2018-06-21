{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StrictData         #-}
{-# LANGUAGE TypeOperators      #-}

module Lib
    ( Account(..)
    , AccountId(..)
    , Transfer(..)
    , API
    , api
    , runServer
    ) where

import           Control.Concurrent.STM     (STM, TVar, atomically, modifyTVar',
                                             newTVar, newTVarIO, orElse,
                                             readTVar, registerDelay, retry,
                                             writeTVar)
import           Control.Monad              (guard)
import           Control.Monad.IO.Class     (liftIO)
import           Control.Monad.Trans.Reader (ReaderT, ask, runReaderT)
import           Data.Aeson                 (FromJSON, ToJSON)
import           Data.ByteString.Lazy.Char8 (ByteString, intercalate, pack)
import qualified Data.Map                   as Map
import           Data.Monoid                ((<>))
import           Data.Traversable           (traverse)
import           GHC.Generics               (Generic)
import           Network.Wai.Handler.Warp   (run)
import           Servant

-- Types

newtype AccountId = AccountId Integer
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

data Account = Account
  { _id      :: AccountId
  , _balance :: Integer
  } deriving (Show, Generic, ToJSON, FromJSON)

data Transfer = Transfer
  { _fromAccount :: AccountId
  , _toAccount   :: AccountId
  , _amount      :: Integer
  } deriving (Show, Generic, ToJSON, FromJSON)

data Config = Config
  { _masterAccountBalance :: Integer
  , _transferTimeout      :: Int
  }

instance FromHttpApiData AccountId where
  parseUrlPiece p = AccountId <$> parseUrlPiece p

instance ToHttpApiData AccountId where
  toUrlPiece (AccountId accid) = toUrlPiece accid

type TransferResult = Either [Error] ()
data Error = AccountNotExists AccountId
  | InsufficientFunds AccountId
  | WrongAmount Integer
  | Other

type State = Map.Map AccountId (TVar Account)

data Env = Env
  { _config :: Config
  , _state  :: TVar State
  , _nextId :: TVar Integer
  }

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
  if amount < 0
    then return $ Left [WrongAmount amount]
    else do
      let
        fromAccM = Map.lookup fromId state
        toAccM = Map.lookup toId state
        changeBalance tv a = modifyTVar' tv $ \acc -> acc{_balance = _balance acc + a}
      case (fromAccM, toAccM) of
        (Nothing, Nothing)   -> return $ Left [AccountNotExists fromId, AccountNotExists toId]
        (Nothing, Just _)    -> return $ Left [AccountNotExists fromId]
        (Just _, Nothing)    -> return $ Left [AccountNotExists toId]
        (Just fromTV, Just toTV) -> do
          Account _ fromAmount <- readTVar fromTV
          guard $ fromAmount >= amount
          _ <- changeBalance fromTV (- amount)
          Right <$> changeBalance toTV amount

-- Endpoints

getAccount :: AccountId -> AppM Account
getAccount accid = do
  Env _ s _ <- ask
  acc <- liftIO $ atomically $ do
    state <- readTVar s
    traverse readTVar (Map.lookup accid state)
  case acc of
    Just a  -> return a
    Nothing -> throwError err404

createAccount :: AppM Account
createAccount = do
  Env _ s n <- ask
  liftIO $ atomically $
    do
      state <- readTVar s
      nextId <- readTVar n
      let
        accid = AccountId nextId
        acc = Account accid 0
      tv <- newTVar acc
      writeTVar s $! Map.insert accid tv state
      writeTVar n $ nextId + 1
      return acc

transfer :: Transfer -> AppM Transfer
transfer t@(Transfer from to amount) = do
  Env (Config _ transferTimeout) s _ <- ask
  result <- liftIO $ do
    timer <- registerDelay transferTimeout
    atomically $ do
      state <- readTVar s
      let
        timeout = do
          b <- readTVar timer
          if b then
            return $ Left [Other]
          else retry
      (transferMoney state from to amount) `orElse` timeout
  case result of
    Right ()   -> return t
    Left errs -> throwError err400{errBody = intercalate "," $ showError <$> errs}

-- Helpers

showError :: Error -> ByteString
showError (AccountNotExists accid) = pack $ "Account '" <> show accid <> "' doesn't exist"
showError (InsufficientFunds accid) = pack $ "Account '" <> show accid <> "' has insufficient funds"
showError (WrongAmount amount) = pack $ "Amount '" <> show amount <> "' is incorrect"
showError Other = "Something went wrong"

-- Server

api :: Proxy API
api = Proxy

server :: ServerT API AppM
server = (getAccount :<|> createAccount) :<|> transfer

app :: Env -> Application
app s = serve api $ hoistServer api nt server
  where
    nt :: AppM a -> Handler a
    nt x = runReaderT x s

runServer :: Integer -> IO ()
runServer port = do
  let config = Config 1000000 1000000
  let masterAccId = AccountId 0
  (state, n) <- liftIO $ do
    masterAcc <- newTVarIO $ Account masterAccId $ _masterAccountBalance config
    s <- newTVarIO $ Map.singleton masterAccId masterAcc
    n <- newTVarIO 1
    return (s, n)
  run (fromInteger port) $ app $ Env config state n
