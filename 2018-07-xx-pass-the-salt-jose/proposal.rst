No way JOSE! Lessons for authors and implementers of open standards
===================================================================

Protocol and data format specifications can be ambiguous, insecure
or have other problems.  Programmers and users bear the brunt of
these issues.  Using JOSE as a case study, I'll discuss mistakes for
standards authors to avoid, and demonstrate programming techniques
for mitigating some kinds of problems.

JOSE (JSON Object Signing and Encryption) is a set of IETF standards
for JSON-based cryptographic objects.  You might know it as JWT or
JWS.  It is used in OpenID Connect, ACME, and other protocols.  JOSE
emerged a few years ago and has been causing headaches for the
presenter ever since.

Using JOSE as a case study, this presentation looks at mistakes to
avoid when specifying a data format or cryptographic protocol.
We'll also explore programming techniques for mitigating some kinds
of problems in specifications.  In particular, we will cover:

* the flawed rationale for the JOSE working group
* why JSON is a poor wire format for cryptographic objects
* cryptography issues in the JOSE specifications
* ambiguities and interoperability problems in the specifications
* common vulnerabilities in JOSE libraries
* how library authors can encourage or enforce safe use
* advice for standards authors or working groups

Each topic will culminate in one simple, actionable take-away.

Programming principles and techniques will be demonstrated using
Haskell and its _jose_ library, which is maintained by the
presenter.

Bio
===

Fraser works at Red Hat on the FreeIPA identity management system
and Dogtag Certificate System. He's interested in security,
cryptography and functional programming.  Jalapeño aficionado from
the land Down Under.
