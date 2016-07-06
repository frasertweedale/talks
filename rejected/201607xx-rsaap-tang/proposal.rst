Presentation Title (75 characters)
----------------------------------

Network-bound encryption with Tang and Clevis


Abstract (400 chars)
--------------------

Encrypting secrets (e.g. disk encryption keys) at rest is important,
yet it's often desirable to automate decryption when in a secure
environment (e.g. on the corporate network). We describe
McCallum-Relyea exchange, a key agreement protocol where only one
side learns the secret, and show how Shamir's Secret Sharing can be
used to define complex unlock policies using data from multiple
sources.

Tweet abstract (135 characters)
-------------------------------

Tang and Clevis: software for secure network-bound encryption with
automatic decryption, and arbitrarily complex key unlock policies.


Session detail (2500 characters)
--------------------------------

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

*Tang* [1] is a protocol and software implementation of *network
bound encryption*; that is, automatic decryption of secrets when a
client has access to a particular server on a secure network.  It
uses *McCallum-Relyea exchange*, a novel two-party key agreement
protocol based on Diffie-Hellman that allows Alice to cooperate with
Bob to compute a static secret without Bob (let alone Eve) being
able to learn the secret.

In this talk I will outline the use cases, explain McCallum-Relyea
exchange and provide an overview of the network protocol, design and
implementation of Tang and the client-side key acquisition
framework, *Clevis* [2].  There will be a live demo showing the
setup and operation of Tang and Clevis to provision and then
automatically acquire secrets such as LUKS volume keys and TLS
private keys in Apache.  The talk will conclude with a discussion of
assumptions, limitations and threats in the Tang protocol, and how
Clevis uses the *Shamir's Secret Sharing* algorithm to support
arbitrarily complex unlock policies.

[1] https://github.com/latchset/tang
[2] https://github.com/latchset/clevis


Topic Category
--------------

Data Security


Keywords
--------

- cloud security
- cryptography
- password management
- risk management
- storage security
