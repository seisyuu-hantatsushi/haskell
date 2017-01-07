import System.IO
import System.Environment

import JpegDecoder 

import qualified Data.ByteString.Lazy as BSL

main :: IO ()
main = do
  args <- getArgs
  input <- BSL.readFile (args!!0)
  output <- openFile (args!!1) WriteMode
  r <- runJpegDecoder input output
  reportResult r
  hClose output
  return ()

reportResult :: Either String () -> IO ()
reportResult (Right ()) = putStrLn "success"
reportResult (Left e) = putStrLn $ "failed:" ++ (show e)
