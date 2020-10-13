# rsa-haskell
bone headed inmplementation of rsa in haskell

## Generate public and private keys
```
./main p1 p2
```
where `p1` and `p2` are large primes generated however you want (We do not do the prime generation).

Creates two files `private.rsa` and `public.rsa`.

## Encrypt data
```
./main e ./clear.txt public.rsa >> encrypted.txt
```
where `public.rsa` is the public key generated above.

## Decrypt data
```
./main d ./encrypted.txt private.rsa >> plain.txt
```
where `private.rsa` is the private key generated above.
