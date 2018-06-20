import           Control.Concurrent  (forkIO, threadDelay)
import           Control.Monad       (forM, void)
import           Network.HTTP.Client (Manager, defaultManagerSettings,
                                      newManager)
import           Servant.API
import           Servant.Client

import           Lib

main :: IO ()
main = do
  _ <- forkIO $ runServer 8081
  let (getAccountE :<|> createAccountE) :<|> transferE = client api
  manager <- newManager defaultManagerSettings

  let master = AccountId 0
  Right (Account accid _) <- execute manager createAccountE

  let
    to = void $ execute manager $ transferE $ Transfer master accid 10
    from = void $ execute manager $ transferE $ Transfer accid master 10

  _ <- forM [1..100] $ \_ -> forkIO to
  _ <- forM [1..100] $ \_ -> forkIO from

  _ <- threadDelay 10000000

  newMaster <- execute manager $ getAccountE master
  newAcc <- execute manager $ getAccountE accid

  print newMaster
  print newAcc

  return ()
  where
    execute :: Manager -> ClientM a -> IO (Either ServantError a)
    execute manager req = runClientM req (ClientEnv manager (BaseUrl Http "localhost" 8081 ""))
