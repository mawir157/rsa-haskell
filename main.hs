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
  putStrLn ("usage: " ++ name ++ " num1 <num2>") 
  putStrLn "  - one argument:"
  putStrLn "     preview the circuit for quantum integers of that length"
  putStrLn "  - two arguments:"
  putStrLn "     simulate the circuit for adding the two numbers"
  putStrLn "     (note that quantum simulation is not efficient)"


main = do
  args <- getArgs
  case parseArgs args of
    Usage -> usage
    PubKey p1 p2 -> do
      g <- newStdGen
      let (f,_) = randomR (0.5 :: Float, 1.0 :: Float) g
      -- (private, public)
      let ((n1,e),(n2,d)) = makeKeyPair (p1, p2, f)
      writeFile "./public.rsa" $ (show n1) ++ "\n" ++ (show e)
      writeFile "./private.rsa" $ (show n2) ++ "\n" ++ (show d)
    CliRSA mode input keyfile -> do
      f <- readFile keyfile
      let [n,d] = map (read) (lines f) :: [Integer]
      g <- readFile input
      let oldText = cleanMessage $ lines g
      if mode == "d"
        then
          do
            -- let newText = decrypt (n,d) alpha oldText
            putStrLn $ decrypt (n,d) alpha oldText
      else if mode == "e"
        then
          do
            -- let newText = encrypt (n,d) alpha oldText
            putStrLn $ encrypt (n,d) alpha oldText
      else error("stop!")
