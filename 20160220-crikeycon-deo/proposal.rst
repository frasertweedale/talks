Presentation Title
------------------

Network Bound Encryption with Deo


Detailed summary
----------------

Full disk encryption and, more generally, encryption of secrets at
rest are part of the information security doctrine.  Unfortunately
these practices have costs in the form of latency (downtime),
repetition (productivity loss), proneness to error (typos; "was that
'1' or 'l'?") or other challenges in supplying a decryption
passphrase when needed (e.g. headless systems).  These hurdles,
aside from the inherent costs, have resulted in an alarming
incidence of advice on the web to, e.g. leave TLS private keys
sitting around in the clear.

We can do better.

Deo (from δεω, *to bind*) is a protocol and software implementation
providing *network bound encryption*; that is, automatic decryption
of secrets when a client has access to a particular Deo server on a
secure network.  It uses *McCallum-Relyea exchange*, a novel
two-party protocol based on ElGamal encryption that allows Alice to
cooperate with Bob to encrypt and decrypt a secret without Bob (let
alone Eve) being able to learn the secret.

In this talk I will outline the use cases, explain McCallum-Relyea
exchange and provide an overview of the network protocol, design and
implementation of Deo.  There will then be live demos of setup and
operation of Deo to automatically decrypt LUKS volume keys and TLS
private keys in Apache.  We will conclude by discussing limitations
and threats in the Deo protocol, and how Deo can play a part in more
complex key policies (wherein *Shamir's Secret Sharing* makes a
cameo appearance).


Name
----

Fraser Tweedale


Bio
---

Fraser is a software engineer at Red Hat where he works on FreeIPA
(identity management), Dogtag (X.509 PKI) and related security
projects.  By night he programs in Haskell and is exploring theorm
proving, dependent types, category theory and other exciting
intersections of mathematics and computer science.
