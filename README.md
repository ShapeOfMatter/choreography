---
title: PCL
subtitle: Probabilistic Choreography Language
author: Mako Bates
---

Wishlist:

- Operations:
  - send
  - input
  - flip
  - output
  - xor, and
  - OT
  - encrypt:
    - vector keys and ciphers
    - bit plaintext
    - enc(b, k): generate c at random, add ((c,k), b) to global lookup.
    - dec(c,k): lookup (c,k) in table, return b or random
      (or error, compare against the dificultly of yao's point-n-permute).
    - [Read for ideas](https://joyofcryptography.com/pdf/chap7.pdf)
- Gathers views during evaluation.
- Gathers output for each party.
- Typechecking yields dimensions of input (and output and views)
- Typechecking yields party list.
