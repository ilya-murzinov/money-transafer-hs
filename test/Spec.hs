import           Control.Concurrent       (forkIO)
import           Control.Concurrent.Async (mapConcurrently)
import           Control.Monad            (void)
import           Network.HTTP.Client      (Manager, defaultManagerSettings,
                                           newManager)
import           Servant.API
import           Servant.Client

import           Lib

main :: IO ()
main = do
  _ <- forkIO $ runServer 8081
  let (getAccountE :<|> createAccountE) :<|> transferE = client api
  m <- newManager defaultManagerSettings

  let master = AccountId 0
  Right (Account accid _) <- execute m createAccountE

  let
    to = void $ execute m $ transferE $ Transfer master accid 10
    from = void $ execute m $ transferE $ Transfer accid master 10

  _ <- mapConcurrently (\i -> if even i then to else from) ([1..100] :: [Integer])

  newMaster <- execute m $ getAccountE master
  newAcc <- execute m $ getAccountE accid

  print newMaster
  print newAcc

  return ()
  where
    execute :: Manager -> ClientM a -> IO (Either ServantError a)
    execute m req = runClientM req (ClientEnv m (BaseUrl Http "localhost" 8081 ""))
