Clevis and Tang: securing your secrets at rest
==============================================

Full disk encryption and, more generally, encryption of secrets at
rest are essential tools in the security toolbox.  But deploying
encryption at rest can have costs: latency (downtime), repetition
(productivity loss), proneness to error (typos; "was that '1' or
'l'?"), challenges in supplying a passphrase when needed (e.g.
headless systems).  Automated decryption often relies on delivery of
escrowed keys (a third party knows your secret).

We can do better.

_Tang_ [1] is a protocol and (along with the client-side program
_Clevis_ [2]) software implementation of *network bound encryption*;
that is, automatic decryption of secrets when a client has access to
a particular server on a secure network.  It uses McCallum-Relyea
exchange, a two-party key computation protocol based on
Diffie-Hellman where only the client can compute the key!  _Clevis_
[2] uses the amazing *Shamir's Secret Sharing* algorithm to
implement unlock policies with thresholds that can include
passphrases, Tang servers and TPM-sealed secrets.

In this talk I will outline the use cases, explain the algorithms
and demonstrate these tools.  The live demo will set up a machine to
automatically decrypt a LUKS volume when a required number of Tang
servers are available.  I will conclude with a discussion of
limitations, assumptions and threats.

[1] https://github.com/latchset/tang
[2] https://github.com/latchset/clevis

Private abstract
----------------

Tang and Clevis are open source projects, available in popular Linux
distributions and (of course) in source form.  I am not the author
but contributed code review and bug fixes in the early stages. 
