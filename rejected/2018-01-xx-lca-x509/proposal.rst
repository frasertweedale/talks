X.509: Beyond TLS
=================

Target audience: Developer


Abstract
--------

Most developers and admins know about X.509 as the certificate
format used in TLS. The push to encrypt the web means that web
developers and administrators are using digital certificates more
than ever. But did you know that there are many other X.509 use
cases?  This talk will show you some certificate use cases you never
imagined!

We will begin with a review of public key cryptography fundamentals.
Then we will examine the anatomy of X.509 certificates and chains of
trust, and discuss how certificates are used in TLS.

Moving beyond TLS, we will explore use cases including S/MIME for
email, Smart Card-based system login and Kerberos PKINIT.   There
will be practical demonstrations of some of these applications.
We'll discuss how FreeIPA's certificate management features address
these use cases.  (FreeIPA is an open-source identity management
system.)

The talk will conclude with a discussion of recent developments in
certificate security.


Private abstract
================

Depending on time available, the demos in order of priority will be:

1. System login
2. S/MIME
3. Kerberos PKINIT

The "recent developments in certificate security" to be discussed at
the end of the talk will include Certificate Transparency (CT), HTTP
Strict Transport Security (HSTS), public key pinning (HSTS et al)
and Let's Encrypt / ACME.
