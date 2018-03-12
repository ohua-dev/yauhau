#! stack runhaskell


import           System.Directory
import           System.Process


leinInstall, etlasBuild, genProjectConfig :: IO ()
leinInstall = callProcess "lein" ["install"]
etlasBuild = callProcess "etlas" ["build"]
genProjectConfig = callProcess "stack" ["runhaskell", "GenProjectClj.hs"]
etlasClean = callProcess "etlas" ["clean"]


main = do
  withCurrentDirectory "../../jvm-runtime" $
    leinInstall
  withCurrentDirectory "../../jvm-integration" $ do
    etlasClean
    etlasBuild
    genProjectConfig
    leinInstall
  etlasClean
  callProcess "etlas" ["sandbox", "hc-pkg", "--", "--force", "unregister", "ohua-jvm-integration"]
  callProcess "etlas" ["sandbox", "hc-pkg", "--", "--force", "unregister", "yauhau-transform"]
  callProcess "etlas" ["install", "--dependencies-only"]
  etlasBuild
  genProjectConfig
