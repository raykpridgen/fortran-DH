# Diffie-Hellman Key Exchange

Alice, Bob communicating
Eve trying to listen

1. Alice, Bob
- Decide on p, g
- p is a sufficiently large prime, found safely with p = 2*q + 1 where q is also large prime
- g is in mult. group mod p, has order q (from above). Found safely with g = h^2 mod p (random h)


2. Alice 
- computes public A = g^a mod p
- A is Alice's public key, a is Alice's private key
- Alice sends Bob g, p, A

3. Bob 
- computes public B = g^b mod p with given g and p
- computes shared key K = A^b mod p
- Bob sends Alice B

4. Alice
- compues shared key K = B^a mod p


# RSA
- Select p, q primes, p =/= q
- n = p * q
- phi(n) = (p - 1)(q - 1)
- select e: gcd(phi(n), e) = 1, 1 < e < phi(n)
- select d: d * e mod (phi(n)) = 1
- Public key: {e, n}
- Private key: {d, n}

## Encrypt
- Plaintext: M < n
- Ciphertext: C = M^e mod n

## Decrypt
- M = C^d mod n 