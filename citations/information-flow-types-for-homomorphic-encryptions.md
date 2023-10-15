---
title: "Information-Flow Types for Homomorphic Encryptions; 2011"
date: "2023-09-20"
tags:
  - information_flow
  - security
  - cryptography
  - type_system
  - homomorphic_encryption
citation:
  author: "Cédric Fournet, Jérémy Planul, Tamara Rezk"
  title: "Information-Flow Types for Homomorphic Encryptions"
  howpublished: "CCS’11, October 17–21, 2011, Chicago, Illinois, USA."
  year: 2011
  where: "Microsoft; Inria"
  url: https://www.microsoft.com/en-us/research/wp-content/uploads/2017/01/information-flow-types-for-homomorphic-encryptions-ccs11.pdf
---

The authors use double-lattice security/information-flow typing (confidentiality & integrity)
to enforce security in protocols involving (potentially nested) encryption, decryption, 
and key-handling operations. 
Critically, as far as I can tell the task of showing that a given {en|de}crytpion scheme
has its respective security property (CPA, CCA2, blindness, homeomophicness) is external to the type system;
the type system enforces security of _use_.

