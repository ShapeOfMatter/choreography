

## Introduction

### Motivation

As in all kinds of engineering; in cryptography it is critical to know that the thing you are building actually does what you intend.
In cryptography however, the intended behavior is often expressed as a negation,
_e.g._ secret information should _never_ be derivable from such'n'such surface of the system.
Observing of the system's behavior provides limited evidence that the system is actually secure in the intended ways.
Formal proofs of security are a popular and well-suited solution to this problem, however:

1. As consumers, we are concerned that a great deal of cyber-security software has been and is being produced without formal verification.
2. As engineers, we would like to not waste time attempting to prove the security of systems which, unbeknownst to us, are not actually secure.
3. As product managers, we would like to have sanity checks
   that complicated software systems actually have the security properties their constituent source-code claims to be proven to have.

In the work we particularly focus on Secure Multi-Party Computation (MPC),
a cryptographic primitive with wide-ranging applications but frustrating performance limitations.
Substantial work has going into improving the performance of MPC systems [citation].
Additionally, in some cases performance can be improved by using a bespoke protocol instead of a general-purpose MPC system [do we have any evidence of this?].
We hope to accelerate this work by providing a tool, comparable to a property-based test, for conveniently spot-checking the security of an MPC implementation.

### Contributions

In this work we present an evaluate a statistical test of the security of MPC protocols.
First we show how MPC security relates to inference and equivalence of probabilistic distributions,
and then we demonstrate that existing tests of distributional equivalence can, with minor modification, be used to refute MPC security.
(We believe that) our tool is adaptable to the wide range of cryptographic security properties that rely on a notion of distributional equivalence,
and our tool is applicable to any software system that
A) can be run 10k-1M times without great inconviencence, and
B) allows certain intermediate values (of which consumed randomness is the least convenient) to be captured for analysis.

## Background

### MPC security

### Inference in PPLs

### Property-based testing


## Related Work

### Verification of MPC

[Computer-aided proofs for multiparty computation with active security](https://arxiv.org/pdf/1806.07197.pdf)

### Languages built inside of MPC implementations

### PPLs for Inference

dice

### PPL proof assistants

lilac

### Probabilistic tests of distributional equality


## The tool in theory


## The tool in practice

### A Choreography Language

### Random generation of programs

### Performance analysis


## Future Work


## Conclusions

