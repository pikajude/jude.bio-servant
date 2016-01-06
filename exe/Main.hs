import JudeWeb                              (serveApp)
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.Gzip
import Network.Wai.Middleware.RequestLogger

main :: IO ()
main = do
    s <- serveApp
    run 8081 $ gzip def s
