No way JOSE! Lessons for crypto standards & libs
================================================

JOSE (JSON Object Signing and Encryption) is a set of IETF standards
for JSON-based cryptographic objects. You might know it as JWT or
JWS. JOSE appeared a few years ago and has been giving me a headache
ever since.

This is a case study of mistakes to avoid when defining a crypto
standard, and how library authors can mitigate the damage. We will
examine:

- the flawed rationale for the JOSE working group
- why JSON is a poor wire format for cryptographic objects
- cryptographic issues in the specifications
- ambiguities and interop problems in the specifications
- common vulnerabilities in JOSE libraries
- how library authors can avoid problems and encourage safe use
- advice for standards authors / working groups

Principles will be demonstrated using the Haskell *jose* library.


Summary (< 100 chars)
---------------------

How library authors can smooth the rough edges and avoid pitfalls in
cryptography standards.


Bio
---

Fraser works at Red Hat on the FreeIPA identity management system
and Dogtag Certificate System. He's interested in security,
cryptography and functional programming. Jalapeño aficionado.
