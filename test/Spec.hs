import           Control.Concurrent        (forkIO)
import           Control.Concurrent.Async  (mapConcurrently)
import           Control.Monad             (void)
import           Network.HTTP.Client       (Manager, defaultManagerSettings,
                                            newManager)
import           Servant.API
import           Servant.Client
import           Test.QuickCheck.Modifiers
import           Test.QuickCheck.Monadic
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck

import           Lib

main :: IO ()
main = do
  _ <- forkIO $ runServer $ fromIntegral port
  m <- newManager defaultManagerSettings
  defaultMain $ tests m

tests :: Manager -> TestTree
tests m = testGroup "All tests" [properties m, unitTests m]

properties m = testGroup "Properties"
  [ testProperty "negative amount" $ \i -> prop_NegativeAmount m i
  , testProperty "not enough money" $ \i -> prop_NotEnoughMoney m i
  , testProperty "single transfer" $ \i -> prop_SingleTransfer m i
  ]

unitTests m = testGroup "Unit tests"
  [ testCase "concurrent test" $ do
    accId <- createAccount m
    Account _ masterBalance <- getAccount m masterId
    let
      mkTransfer i =
        if even i
          then void $ transfer m accId masterId 10
          else void $ transfer m masterId accId 10

    _ <- mapConcurrently mkTransfer ([1..10] :: [Integer])

    Account _ updatedMasterBalance <- getAccount m masterId
    Account _ updatedAccBalance <- getAccount m accId
    updatedMasterBalance @?= masterBalance
    updatedAccBalance @?= 0
  ]

prop_NegativeAmount :: Manager -> NonNegative Integer -> Property
prop_NegativeAmount m (NonNegative amount) = monadicIO $ do
  accId <- run $ createAccount m
  Account _ masterBalance <- run $ getAccount m masterId
  Left (FailureResponse err) <- run $ transfer m masterId accId (-amount)
  return True

prop_NotEnoughMoney :: Manager -> NonNegative Integer -> Property
prop_NotEnoughMoney m (NonNegative amount) = monadicIO $ do
  accId <- run $ createAccount m
  Account _ masterBalance <- run $ getAccount m masterId
  Left (FailureResponse err) <- run $ transfer m accId masterId amount
  return True

prop_SingleTransfer :: Manager -> Positive Integer -> Property
prop_SingleTransfer m (Positive amount) = monadicIO $ do
  accId <- run $ createAccount m
  Account _ masterBalance <- run $ getAccount m masterId
  Right _ <- run $ transfer m masterId accId amount
  Account _ updatedMasterBalance <- run $ getAccount m masterId
  Account _ updatedAccBalance <- run $ getAccount m accId
  _ <- run $ transfer m accId masterId amount
  return $
    updatedMasterBalance == masterBalance - amount
    && updatedAccBalance == amount

createAccount :: Manager -> IO AccountId
createAccount m = do
  Right (Account accId _) <- execute m createAccountE
  return accId

getAccount :: Manager -> AccountId -> IO Account
getAccount m accId = do
  Right acc <- execute m $ getAccountE accId
  return acc

transfer :: Manager -> AccountId -> AccountId -> Integer -> IO (Either ServantError Transfer)
transfer m from to amount = execute m $ transferE $ Transfer from to amount

execute :: Manager -> ClientM a -> IO (Either ServantError a)
execute m req = runClientM req (ClientEnv m (BaseUrl Http "localhost" port ""))

(getAccountE :<|> createAccountE) :<|> transferE = client api

masterId = AccountId 0

port = 8081
