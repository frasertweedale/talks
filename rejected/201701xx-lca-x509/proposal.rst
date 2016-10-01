X.509: Beyond TLS
=================

Target audience: Developer


Abstract
-------

You probably know about X.509 as the certificate format used in
TLS/SSL.  The push to encrypt the web means that web developers and
server administrators are using digital certificates more than ever,
but did you know that there are several other X.509 use cases?  This
talk will show you how X.509 certificates are used in ways you never
imagined!

Starting with some public key cryptography fundamentals, we will
examine the anatomy of X.509 certificates and chains of trust.  From
there I will outline how certificates are used to secure TLS, and
explain client certificate authentication.

Moving beyond TLS, we will explore some exotic use cases including
S/MIME for email privacy and authentication, Smart Card-based system
logon and Kerberos PKINIT, before circling back to the Web to
discuss some important certificate extensions and related
technologies for increased security when using certificates.

The talk will conclude with a discussion of how the certificate
management features in FreeIPA (an open-source identity management
system) can be used to handle the use cases discussed, and more!


Private abstract
----------------

I hope to perform live demonstrations of most of the X.509
applications covered in the talk, including client certificate
authentication, S/MIME and smart card logon.
