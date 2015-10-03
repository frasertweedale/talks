Presentation Title
------------------

Network Bound (Disk) Encryption with Deo


Detailed summary
----------------

Disk encryption when used correctly is an important tool for keeping
sensitive data safe.  For organisations, policy may (should?)
require disks to be encrypted, or may prevent unencrypted disks from
being discarded or serviced under warranty.

Password-based disk encryption might meet security needs but doesn't
scale when it comes to booting.  Even if a user is present, entering
a secure passphrase takes time and is prone to error.

Deo is a protocol and software providing *network bound encryption*,
using the X.509 PKI.  A stateless server advertises an encryption
certificate and responds to decryption requests.  Clients use the
encrytion certificate to encipher secrets and contact the server to
request decryption.  Hence, decryption is only possible while the
client remains on the same network as the Deo server.  TLS is used
for all network communication, and plugins provide general query,
encryption and decryption commands and LUKS integration.

This talk will explain the use cases, protocol, design and
implementation of Deo.  Automated LUKS-based disk decryption will be
demonstrated.  I will conclude with a discussion of upcoming
improvements and possible future directions.


Name
----

Fraser Tweedale


Bio
---

Fraser is an identity management engineer at Red Hat where he works
on FreeIPA, Dogtag Certificate System and related identity and
security projects.  By night he programs in Haskell, proves theorems
in Coq and is exploring dependent types, category theory and other
exciting intersections of mathematics and computer science.


Description of demonstrations
-----------------------------

Deo's functionality will be demonstrated on my computer using
virtual machines and virtual networks.  A Deo decryption server will
be started, and a client system will be set up to use this Deo
server for decryption of disk encryption keys.  It will be shown
that automatic disk encryption fails when the Deo server is not
available (falling back to password-based decryption if configured).

If time permits, other features of Deo such as key rollover may be
demonstrated.


Prior presentation of material
------------------------------

Deo is new software and to best of my knowledge will not have been
presented in detail at a conference prior to Ruxcon.

The software is available on GitHub:
https://github.com/npmccallum/deo

A high-level design document is available:
http://www.freeipa.org/page/Network_Bound_Disk_Encryption

A short demo is available at YouTube:
https://www.youtube.com/watch?v=lyDmhhVgXEc
