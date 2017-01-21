X.509 and TLS fundamentals
==========================

*Transport Layer Security* (TLS; formerly *SSL*) is a foundational
internet security protocol providing privacy, integrity and
authentication via the X.509 public-key certificate infrastructure.
We rely on these technologies every day, but how much do you really
know about how they work - or what to do when they don't work?

In this talk we will review some public-key cryptography
fundamentals before investigating the anatomy of X.509 certificates
and chains of trust.  Then we will explore how TLS sets up a secure
connection - the *TLS handshake*, and how X.509 certificate
validation is performed in this context.

Finally we will discuss some common failure modes for these
technologies, and useful tools for diagnosing TLS or certificate
issues.
