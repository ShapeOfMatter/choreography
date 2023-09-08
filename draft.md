

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
We accelerate this work by providing a tool, comparable to a property-based test,
for conveniently spot-checking the security of an MPC implementation.

### Contributions

In this work we present and evaluate a statistical test of the security of MPC protocols.
First we show how MPC security relates to inference and equivalence of probabilistic distributions,
and then we demonstrate that existing tests of distributional equivalence can, with minor modification, be used to refute MPC security.
Our tool is adaptable to the wide range of cryptographic security properties that can be defined in terms of distributional equivalence,
and our tool is applicable to any software system that
A) can be run 10k-1M times without great inconvenience, and
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

[decision trees paper]()

## The tool in theory

... Position the equivalence in the definition of MPC security...

To actually test a protocol, one uses two identical machine learning (ML) tools to predict the honest secrets based on the corrupt views.
One of the two models has access to the "real world" views, and the only has access to the "ideal world" views.
In principal, _any_ ML system is ok to use; the choice is partly an engineering question
and partly a judgment call about what kinds of systems will be best able to discern patterns in the views of the subject protocol.
Generating the data for these tests requires running the protocol many times.
The process is enumerated in Algorithm 1.

Note that the data must be understood as vectors/matricies/tensors of bits. [is this actually important? are there exceptions/precondidtions?]

> **Algorithm 1**
>
> 1. Set a chosen number of iterations `iters`,
>    number of rows of data on which to train each model `trainN`,
>    and number of rows of data on which to test each model `testN`.
> 2. Choose $\alpha$, your probability of randomly flagging a secure protocol as insecure.
> 3. For `i <- [1..iters]`, calculate scores for the real and ideal models:
>    1. Run the protocol `trainN` times, collecting the corrupt views and honest secrets.
>    2. Train the first ML model on the collected data.
        The real-world views are the input features, and the honest secrets are the labels. [I think this is generic ML terminology?]
>    3. Train the second ML model the same way, but occlude (omit or overwrite) view data to only reveal the ideal-world view.
>    4. Run the protocol `testN` more times, again collecting the relevant data.
>    5. The score for each model is the $L^1$ distance between the testing labels and the respective model outputs
        (_i.e._ the number of bits wrong when run on the `testN` rows of test data).
> 4. Using the `iters` samples of "real-world" and "ideal-world" scores,
     calculate the $p$-value for the hypothesis that the distribution of real-world scores is better on average than the ideal-world distribution.
     If this is significant ($p \leq \alpha$), output `INSECURE`, otherwise output `SECURE`.

### Issue: false positives

What our tool actually invalidates is that the views are uniformly random within the slice compatible with the obseved inputs and outputs. 


## The tool in practice

### A Choreography Language

### Random generation of programs

### Performance analysis


## Future Work


## Conclusions

