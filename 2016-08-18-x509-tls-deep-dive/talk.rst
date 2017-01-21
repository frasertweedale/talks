..
  Copyright 2016  Red Hat, Inc.

  This work is licensed under the Creative Commons Attribution 4.0
  International License. To view a copy of this license, visit
  http://creativecommons.org/licenses/by/4.0/.


***********************
Public-key cryptography
***********************

Key generation
--------------

.. raw:: latex

  \begin{center}
  \resizebox{!}{0.8\paperheight}{
    \input{Public_key_making-ARTIFACT.pdf_tex}
  }
  \end{center}


Signing
-------

.. raw:: latex

  \begin{center}
  \resizebox{!}{0.8\paperheight}{
    \input{Public_key_signing-ARTIFACT.pdf_tex}
  }
  \end{center}


Key agreement
-------------

.. raw:: latex

  \begin{center}
  \resizebox{!}{0.8\paperheight}{
    \input{Public_key_shared_secret-ARTIFACT.pdf_tex}
  }
  \end{center}


***
TLS
***


*Transport Layer Security (TLS)*
--------------------------------

- f.k.a. *Secure Sockets Layer (SSL)*

- Used *everywhere* to secure communications

- Common implementations: OpenSSL, NSS, GnuTLS

- Latest version: TLS 1.2 (`RFC 5264`_)

.. _RFC 5264: https://tools.ietf.org/html/rfc5246


TLS goals
---------

- Authenticate server

- Authenticate client (optional)

- Establish secure channel
  - *privacy* and *integrity*


TLS *handshake*
---------------

- Initiated by client

- Server sends an *X.509 certificate* to prove identity

- Client and server perform *key agreement*

- Handshake ends; key is used to secure subsequent communications
  - *Application protocol* e.g. HTTP


Cipher suite
------------

- Client and server have supported *cipher suites*

- Client advertises its cipher suites in ``ClientHello`` message

- ``ServerHello`` response declares chosen cipher suite

- Server cipher suites are (usually) configurable


Key agreement protocols
-----------------------

- RSA

- Diffie-Hellman (DH)

- Ephemeral Diffie-Hellman (DHE)
  - provides *forward secrecy*

- *Elliptic curve (EC)* DH variants (`RFC 4492`_)

- TLS 1.3 will have (EC)DHE *only*

.. _RFC 4492: https://tools.ietf.org/html/rfc4492


Ciphers (common)
----------------

- AES CBC
- AES GCM (`RFC 5285`_)
- RC4 (prohibited by `RFC 7465`_ but still widespead)

.. _RFC 5285: https://tools.ietf.org/html/rfc5289
.. _RFC 7465: https://tools.ietf.org/html/rfc7465


Handshake failures
------------------

- Client and server cannot agree on cipher suite
  - Windows XP

- Client does not support large-enough DH parameters
  - Java 6

- Certificate for wrong server sent
  - No SNI (Windows XP, Java 6)

- Certificate expired or otherwise invalid


*Server Name Indication (SNI)*
------------------------------

- Enables multiple TLS servers to be hosted on single TCP endpoint

- Client sends SNI *extension* in ``ClientHello``

- Defined in `RFC 6066`_

.. _RFC 6066: https://tools.ietf.org/html/rfc6066


*****
X.509
*****

X.509
-----

- Digital certificate format (`RFC 5280`_)

- Binds a *public key* to a *subject* (identity)

- Signed by a *certificate authority (CA)*

- TLS server send an *end-entity (EE)* certificate
  - a.k.a. *leaf certificate*

- Client looks for *certification path* from trusted CAs to server
  certificate
  - a.k.a. *chain of trust*

.. _RFC 5280: https://tools.ietf.org/html/rfc5280


X.509 anatomy
-------------

- *Issuer Distinguished Name*
- Serial number
- Validity period
- *Subject Distinguished Name*
- Subject public key
- Extensions
- Signature


X.509 extensions
----------------

- Subject Alternative Name (SAN)
- Subject / Authority Key Identifiers
- (Extended) Key Usage
- CRL / OCSP locators
- Certificate constraints / policies


X.509 validity
--------------

- Certificates have a validity period (typically ≤ 2y)

- Clients reject expired certificates **anywhere in certification
  path**

- There is a push towards shorter-lived certificates and automated
  renewal
  - `*Let's Encrypt*`_ / `ACME`_

.. _*Let's Encrypt*: https://letsencrypt.org/
.. _ACME: https://datatracker.ietf.org/wg/acme/documents/


Certificate subject checking
----------------------------

- TLS client tries to match server's certificate with DNS name

- Checks DNS names in SAN extension

- Checks *Common Name (CN)* in *Subject DN* (**deprecated**)

- Wildcard usually permitted in leftmost position (**deprecated**)

- Best practices: `RFC 6125`_

.. _RFC 6125: https://tools.ietf.org/html/rfc6125

..
  - SAN can also contain:
    - IP addresses (IaaS applications)
    - other kinds of identifiers (non-TLS applications)


Certificate revocation
----------------------

- CAs can revoke certificates they issued

- ***Certificate Revocation Lists (CRLs)***

  ..  - on a period basis e.g. daily, hourly

- ***Online Certificate Status Protocol (OCSP)***
  (`RFC 6960`_)

.. _RFC 6960: https://tools.ietf.org/html/rfc6960

- TLS clients might check CRL, OCSP, neither or both


OCSP stapling
-------------

- TLS client can ask server for OCSP response during TLS handshake

- `RFC 6066`_ (single response) and `RFC 6961`_ (multiple responses)

- *TLS Feature* X.509 extension to **require** stapling (`RFC 7633`_)
  - a.k.a. *OCSP Must-Staple*

.. _RFC 6961: https://tools.ietf.org/html/rfc6961
.. _RFC 7633: https://tools.ietf.org/html/rfc7633


***************
Troubleshooting
***************

TLS troubleshooting checklist
-----------------------------

#. Cert expired?

#. Cert subject matches client access? (No SNI?)

#. Cert revoked?  Check CRL / OCSP availability.

#. Server sending correct intermediate certs?

#. Client trusts the correct CA?

#. Supported cipher suites compatible?

#. Client supports server DH param size?


TLS troubleshooting tools
-------------------------

- Qualys SSL Server Test
  - https://www.ssllabs.com/ssltest

- `testssl.sh`_

- Wireshark

.. _testssl.sh: https://testssl.sh/


TLS miscellany
--------------

- *Application Layer Protocol Negotiation (ALPN)* (`RFC 7301`_)
  - f.k.a. *Next Protocol Negotiation (NPN)*

- Session resumption / PSK tickets

- Kerberos / OpenPGP-based authentication

- *DANE* (DNSSEC-based certificate validation) (`RFC 6698`_)

- *So much other stuff*
  - https://tools.ietf.org/wg/tls/
  - https://tools.ietf.org/wg/pkix/

  ..                       > 100 RFCS!

.. _RFC 7301: https://tools.ietf.org/html/rfc7301
.. _RFC 6698: https://tools.ietf.org/html/rfc6698
