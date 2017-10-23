No way JOSE! Lessons for authors of cryptographic standards and libraries
=========================================================================

*JOSE (JSON Object Signing and Encryption)* is a set of IETF
standards for JSON-based cryptographic objects.  You might know it
as JWT or JWS.  JOSE appeared a few years ago and has been giving me
a headache ever since.

This presentation is a case study in how not to define a
cryptography standard, and how to mitigate the fail as a library
author.  It will have something for everyone, from the crypto
boffins, to offensive types / vuln hunters, to software developers.
We will examine:

- the flawed rationale for the JOSE working group
- why JSON is an inappropriate wire format for cryptographic objects
- cryptographic problems in the specifications
- ambiguities and interoperability problems in the specifications
- common vulnerabilities in JOSE libraries
- how library authors can avoid problems and encourage safe use
- advice for standards authors / working groups

Implementation principles will be demonstrated using the Haskell
*jose* library, authored and maintained by the presenter.


Bio
---

Fraser works at Red Hat on the FreeIPA identity management system
and Dogtag Certificate System. He's interested in security,
cryptography and functional programming. Jalapeño aficionado.
