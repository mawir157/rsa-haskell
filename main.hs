import RSA
import System.Environment
import System.Random

--------------------------------- Command line ---------------------------------
-- | A datatype for the possible command line arguments
data Args = Usage | PubKey Integer Integer | CliRSA String String String

-- | A function to parse the command line arguments
parseArgs :: [String] -> Args
parseArgs [s1,s2,s3] = CliRSA s1 s2 s3
parseArgs [s1,s2] = case (reads s1, reads s2) of
  ([(x1,_)],[(x2,_)]) -> PubKey x1 x2
  _ -> Usage
parseArgs _ = Usage

usage :: IO ()
usage = do
  name <- getProgName
  putStrLn ("usage: " ++ name ++ " prime1 prime2") 
  putStrLn "  - Generates public and private keys based on the primes"
  putStrLn "    (non-deterministic - will generate new keys each time)"
  putStrLn ("usage: " ++ name ++ " [e/d] keyFile.rsa input.txt") 
  putStrLn "  -  encodes or decodes the contents of input.txt"
  putStrLn "     (encode (e) should use public.rsa and decode (d) private.rsa)"

main = do
  args <- getArgs
  case parseArgs args of
    Usage -> usage
    PubKey p1 p2 -> do
      g <- newStdGen
      let (f,_) = randomR (0.5 :: Float, 1.0 :: Float) g
      let ((n1,e),(n2,d)) = makeKeyPair (p1, p2, f)
      writeFile "./public.rsa"  $ (show n1) ++ "\n" ++ (show e)
      writeFile "./private.rsa" $ (show n2) ++ "\n" ++ (show d)
    CliRSA mode input keyfile -> do
      f <- readFile keyfile
      let [n,d] = map (read) (lines f) :: [Integer]
      g <- readFile input
      let oldText = cleanMessage $ lines g
      if mode == "d" then do
            putStrLn $ decrypt (n,d) alpha oldText
      else if mode == "e" then do
            putStrLn $ encrypt (n,d) alpha oldText
      else error("stop!")
