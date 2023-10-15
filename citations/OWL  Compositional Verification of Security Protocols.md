---
title: "OWL: Compositional Verification of Security Protocols via an Information-Flow Type System; 2023"
date: "2023-09-20"
tags:
  - modular_proofs
  - security
  - cryptography
  - composition_of_security
citation:
  author: "Joshua Gancher; Sydney Gibson; Pratap Singh; Samvid Dharanikota; Bryan Parno"
  title: "OWL: Compositional Verification of Security Protocols via an Information-Flow Type System"
  howpublished: "2023 IEEE Symposium on Security and Privacy (SP)"
  year: 2023
  month: May
  where: "Carnegie Mellon University"
  url: https://eprint.iacr.org/2023/473.pdf
---

OWL is distinguished, among other ways, but its use of the "computational model",
_i.e._ everything is done at the level of bits
(and adversaries are arbitrary poly-time turing machines).
OWL is strongly typed, and these types cary the burdon of proof.
The actual guarentee you get is:

> OwlLang guarantees that well-typed protocols satisfy simulatability and correctness.
> Simulatability states that, for any adversary A corrupting a chosen set of names,
> running the protocol cannot leak any more information to A than it had before
> the protocol’s execution.
> Dually, correctness states that all refinements on data in the protocol hold with high probability.

I think their "correctness" is not MPC correctness,
but rather an "almost always" version of normal type-safety.

It's not entierly clear what their simulatabilty rule actually means...
it's game-based, and _sounds_ like MPC...
That said, they do seem to be taking a number of cryptographic primatives as language primatices,
particularly {en|de}cryption, hashing, and DHKE.
Both the semantics and type rules are relatively complicated;
fully understanding this looks like a project.

TODO: figure out if it's true that OWl can only do non-interfeence, not MPC.
