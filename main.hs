import RSA

main = do 
  -- let (privateKey, publicKey)  = makeKeyPair (739, 983, 0.78)
  -- let (privateKey, publicKey)  = makeKeyPair (7349, 8753, 0.66)
  -- let (privateKey, publicKey)  = makeKeyPair (94583, 55333, 0.25)
  let (privateKey, publicKey)  = makeKeyPair (623681, 997057, 0.5123)

  putStrLn $ show publicKey

  let plainText = "This is a test message. It has MANY (3) different, features!"
  let cipherText = encrypt publicKey alpha plainText
  let decodedText1 = decrypt privateKey alpha cipherText
  let (decodedText2, key) = hack publicKey alpha cipherText

  putStrLn $ plainText
  putStrLn $ cipherText
  putStrLn $ decodedText1
  putStrLn $ decodedText2
