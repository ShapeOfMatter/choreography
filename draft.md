

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

Secure Multi-Party Computation (MPC or SMPC, depending on the text)
is a cryptographic process that allows some group of participating parties or machines
to compute a function that depends on all of their private inputs,
without any of them learning anything except the final result.
Implementations that work on arbitrary functions (representable as circuit computations in some field)
have existed since [...],
but their use by the public is limited by their performance.
Some performance limitations are fundamental to the problem
(roughly, every branch of the computation must be executed so that the control-flow does not itself leak information),
but techniques have been found to improve performance somewhat.

The traditional expression of MPC assumes a pre-determined function F, "the functionality",
mapping the tuple of all participants' input vectors to the respective tuple of output vectors
(or to distributions of outputs, if F is non-deterministic).
In an _ideal world_, a trusted third party (the _ideal functionality_) would accept input messages
from all other parties, compute F, and then send the parties their respective outputs.
A _real world_ protocol Π is said to correctly implement F if, for all possible inputs,
Π results in all parties getting the same outputs as they would have in the ideal world.
Defining the security of Π requires a clear threat model, and requires defining the participants' "views".

_Passive Security_ supposes a computationally-bounded adversary
that can see everything happening inside a fixed subset of the participating machines.
This adversary cannot cause the parties to deviate from the protocol Π in any way;
we are interested in whether it can learn anything beyond the outputs of the "corrupted" machines
by observing the execution of Π.
As an example, this notion of security might be of interest if Alice trusted Bob to participate
in a distributed computation with her, but did not trust him to properly store or destroy the log-files
generated.
The _ideal world view_ of a party is what it sees in the ideal world: just its inputs and outputs.
The _real world view_ of a party is everything it sees in the real world while executing its part of Π:
its inputs, outputs, all messages it receives, and all the bits of random tape it uses.

> **Definition 1. Passive MPC Security**
> A protocol Π,
> involving participating parties P,
> that correctly implements functionality F,
> is secure against a passive adversary controlling C⊂P iff
> there exists a poly-time-computable function SIM s.t.
> ....

This definition represents security by a transitive argument:
Since the adversary in the ideal world can compute all the data the real-world adversary starts with,
anything the real-world adversary can compute the ideal-world adversary can also compute!

In practice, security is usually quantified over all possible corruptions C
where |C|≤|P|/x for some x, but this is not fundamental and we only consider a single C at a time.

Definition 1 is tight, in the sense that attempting to relax it invariable either
allows a "secure" protocol to do something bad, or requires introducing restrictions
on the kinds of functionality to which the definition may be applied.
(Of course the idea that a secure protocol _can't_ do anything bad implies an assumption that computing
the functionality is a good thing. [move to footnote])
That said, some simplification is possible for our purposes:
Since the corrupted parties are assumed to follow Π faithfully, we are fine to limit ourselves to
choreographic protocols in which all messages sent are expected and all messages expected are received.
In this context, and because the only source of non-determinism is the parties' random tapes
(which are captured in the real-world views),
we can safely ignore any messages sent _within_ C.
Then we can collapse all the corrupted parties C into a single party C,
and similarly all the other parties into a single party H (for "honest").
Finally, in this work we assume that protocols implement their functionalities _correctly_,
and usually leave the corresponding F implicit.
In other words, we define F to be whatever mapping results from the execution of Π.
This present work describes a system for property-based testing of MPC _security_;
a corresponding test of _correctness_ is trivial for any pre-defined F.

_Active Security_ (sometimes "malicious security")
supposes a computationally bounded adversary that can cause corrupted machines to take arbitrary actions,
breaking the choreography,
forging messages (to such extent as that's actually possible),
lying about computation results,
and/or playing along with the protocol for any period of time.
In contrast to passive adversaries, which are weaker than the threat models most real programmers worry about,
an active adversary subsumes any threat model (including adaptive corruption, depending how it's framed).
The corresponding security definition is a bit more involved, and we omit it for brevity.
Since we're not directly testing for active security in this work,
all else that needs to be said about it is that
any protocol which is not _passive secure_ is automatically not _active secure_.

[Other aspects we may wish to cover:
 vacuous protocols,
 vacuous security,
 infertabilty of functionality,
 comparison to control flow properties,
 review of other work,
 review of stuff we tried that didn't work...
 ]

[ computer proofs of MPC paper; (double-check malicions case...)
  l-obliv
  wysteria et al
  probabilistic separation logic]

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

[How weedy do we want to get here?]

ASAP Wilcoxn vs paired T Test!

### A Choreography Language

Our python script for actually performing the test consumes a stream of fixed-length bit vectors.
(In practice, this is a CSV. Annotation of which columns are available in the real and ideal worlds is provided in the header.)
[Does our tool strictly need the vectors to be fixed width? Could someone do this kind of test on a system with branching control flow?]
To facilitate analysis of the tool, we implemented a choreographic programming language, CHO,
with surrounding tooling to support large batches of parallel evaluations of a single program and piping of the generated data.
The design of CHO is driven by the contextual assumptions of MPC security.
[Do we want a BNF? Semantics?]
In addition to bit-level computations and transmission of single bits between arbitrary parties,
each party has its own read-only tape for secret inputs and random bits
(CHO is also a probabilistic language),
and its own tape for output bits.
CHO supports macros, but they are extremely limited.
Finally, CHO supports 1-of-N oblivious transfer as a primitive operation.
This allows us to test (correct and incorrect) implementations of the classic GMW protocol.

### Random generation of programs

The goal of this work is a tool that can detect mistakes in MPC protocols.
Such a tools is only useful if there if new protocols and implementations are being written.
Therefore, while it is critical to demonstrate that our tool can detect errors manually edite into known-secure protocols,
we _also_ need to show that the tool can detect a lack of security in entirely novel and maximally diverse protocols.
We attempt to show this by randomly generating hundreds [can we say thousands?] of protocols and testing them with different test parameters.

The probability that a randomly generated protocol will be MPC secure is not completely negligible.
As an easy example, if no communicated values ever depend on secret inputs, then the protocol meets the definition of security!
(Never mind that such a protocol is useless; _none_ of the protocols we generated are _useful_.)
That said, for reasonable generator settings we find that _very few_ generated protocols continue to appear secure as we increase the power of the test.

[Probably a lot of the details of the generator go in the appendix?]

### Performance analysis


## Future Work


## Conclusions

