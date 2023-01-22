# Title

Kerberos PKINIT: what, why, and how (to break it)

## Abstract

The Kerberos PKINIT extension replaces password authentication with
X.509 certificates.  This bring some advantages but also new risks.
In this presentation I explain and demonstrate how PKINIT works, and
present a novel attack against FreeIPA's PKINIT implementation.

Kerberos is an authentication and single sign-on protocol based on
symmetric cryptography.  To avoid the drawbacks and risks of
passwords, the PKINIT protocol extension enables clients to
authenticate using public key cryptography and X.509 certificates.
To further improve security, private keys can reside and
signing/decrytion operations can be performed on hardware
cryptographic tokens (smart card, PIV, TPM, etc).

I will start the talk with a brief overview of the core Kerberos
protocol.  Next I will explain how the PKINIT extension works, and
demonstrate how to set up PKINIT and use it in a FreeIPA
environment.

Finally I will discuss some of the risks that arise when using
PKINIT, and security considerations for implementations and
administrators.  I will present and demonstrate a recently
discovered PKINIT security flaw in some older (but still supported)
versions of FreeIPA.
