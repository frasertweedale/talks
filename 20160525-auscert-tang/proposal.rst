Presentation Title
------------------

Network-bound encryption with Tang and Clevis


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

*Tang* [1] is a protocol and (along with the client-side *Clevis*)
software implementation of *network bound encryption*; that is,
automatic decryption of secrets when a client has access to a
particular server on a secure network.  It uses *McCallum-Relyea
exchange*, a novel two-party protocol based on ElGamal encryption
that allows Alice to cooperate with Bob to encrypt and decrypt a
secret without Bob (let alone Eve) being able to learn the secret.

In this talk I will outline the use cases, explain McCallum-Relyea
exchange and provide an overview of the network protocol, design and
implementation of Tang and Clevis.  There will be a live demo
showing the setup and operation of Tang and Clevis to automatically
decrypt LUKS volume keys and TLS private keys in Apache.  The talk
will conclude with a discussion of assumptions, limitations and
threats in the Tang protocol, and how the protocol can play a part
in more complex access policies (wherein *Shamir's Secret Sharing*
makes an appearance).

*Note: Tang and Clevis were formerly a single project called Deo[2].*

[1] https://github.com/npmccallum/tang
[2] https://github.com/npmccallum/deo
