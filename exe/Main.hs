import Data.Maybe
import JudeWeb                              (serveApp)
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.Gzip
import Network.Wai.Middleware.RequestLogger
import System.Environment
import Text.Read

main :: IO ()
main = do
    s <- serveApp
    port <- (>>= readMaybe) <$> lookupEnv "PORT"
    run (fromMaybe 8081 port) $ logStdout $ gzip def s
