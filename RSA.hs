module RSA where

import qualified Data.Bimap as BM
import Data.List.Split (chunksOf)
import Data.Maybe (fromMaybe, fromJust)
import Data.List (nub)

type KeyPair = (Integer, Integer)

type Alphabet = BM.Bimap Integer Char

if' True  x _ = x
if' False _ x = x

getModInv :: Integer -> Integer -> Integer
getModInv n p = mod (t + p) p
  where (_,t,_) = extGCD n p

extGCD :: Integer -> Integer -> (Integer, Integer, Integer)
extGCD 0 b = (b, 0, 1)
extGCD a b = (g, t - (div b a) * s, s)
  where (g, s, t) = extGCD (mod b a) a

-- b^a mod p
modPow :: Integer -> Integer -> Integer -> Integer
modPow b 0 p = 1
modPow b 1 p = mod b p
modPow b a p | even a = mod ((modPow b (div a 2) p) ^ 2) p
             | odd  a = mod ((modPow b (div (a-1) 2) p) ^ 2 * b) p

blockSize :: KeyPair -> Alphabet -> Int
blockSize (n,_) alpha = 
          floor $ logBase (fromIntegral $ BM.size alpha) (fromIntegral n)

strToInteger :: Int -> Alphabet -> String -> Integer
strToInteger bSize alph block = sum $ z
  where n = fromIntegral $ BM.size alph
        a = map (\c -> fromJust $ BM.lookupR c alph) block -- [Integer]
        z = zipWith (*) a (reverse $ take (bSize) (iterate (*n) 1))

integerToStr :: Int -> Alphabet -> Integer -> String
integerToStr 0 _ _ = []
integerToStr r alpha code = [c] ++ integerToStr (r-1) alpha (div code n)
  where n = fromIntegral $ BM.size alpha
        c = fromJust $ BM.lookup (mod code n) alpha

rsa :: KeyPair -> [Integer] -> [Integer]
rsa (n, e) xs = map (\p -> modPow p e n) xs

encrypt :: KeyPair -> Alphabet -> String -> String
encrypt (n, e) alpha plain = concat cy
  where bSize = blockSize (n,e) alpha
        is = map (strToInteger bSize alpha) (chunksOf bSize plain)
        is' = rsa (n,e) is
        cy = map (reverse . integerToStr (bSize+1) alpha) is'

decrypt :: KeyPair -> Alphabet -> String -> String
decrypt (n, d) alpha cipher = concat pl
  where bSize = blockSize (n,d) alpha
        is = map (strToInteger (bSize+1) alpha) (chunksOf (bSize+1) cipher)
        is' = rsa (n,d) is
        pl = map (reverse . integerToStr (bSize) alpha) is'

totient :: Integer -> Integer
totient n = div (n * product g) (product f)
  where f = nub $ factor 2 n
        g = map (\x -> x - 1) f

findCoPrime :: Integer -> Integer -> Integer
findCoPrime x n 
  | (gcd x n) == 1 = x
  | otherwise      = findCoPrime (x-1) n

makeKeyPair :: (Integer, Integer, Float) -> (KeyPair, KeyPair)
makeKeyPair (p1, p2, flt) = ((n,e),(n,d))
  where n = p1 * p2
        phin = n - (p1 + p2) + 1
        t = floor (flt * fromIntegral phin)
        e = findCoPrime t phin
        d = getModInv e phin

publicToKeyPair :: KeyPair -> KeyPair
publicToKeyPair (n, e) = (n, d)
  where v = totient n
        d = getModInv e v 

hack :: KeyPair -> Alphabet -> String -> (String, KeyPair)
hack publicKey alpha cipher = (decrypt privateKey alpha cipher, privateKey)
  where privateKey = publicToKeyPair publicKey

factor :: Integer -> Integer -> [Integer]
factor _ 1 = []
factor p n
  | p * p > n    = [n]
  | mod n p == 0 = [p] ++ (factor p (div n p))
  | otherwise    = factor (nxt p) n
  where nxt n = if' (n == 2) 3 (n+2)

symbols = (
           ['A'..'Z'] ++ ['a'..'z'] ++ 
           [' ', '.', ',', '(', ')', '!', '_', '?', ';', ':', '-'] ++ 
           ['0'..'9'] ++ ['\n', '\"', '\'']
           )

alpha = BM.fromList $ zip [0,1..] symbols

cleanMessage :: [String] -> String
cleanMessage s = filter (\x -> elem x symbols) s'
  where s' = catStrings s

catStrings :: [String] -> String
catStrings (s:ss) = s ++ "\n" ++ catStrings ss
catStrings [] = ""
