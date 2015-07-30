..
  Copyright 2015  Red Hat, Inc.

  This work is licensed under the Creative Commons Attribution 4.0
  International License. To view a copy of this license, visit
  http://creativecommons.org/licenses/by/4.0/.


************
Introduction
************

About me
========

- Identity Management developer at Red Hat

- *Dogtag Certificate System* and *FreeIPA*

- Mostly Python and Java at work

- Mostly Haskell for other projects


*************
Cryptography!
*************

*************
(the library)
*************

Outline
=======

- Background

- Library overview

- Case studies

- Resources


Admonition
==========

- Do not invent your own crypto primitives

- Do not write your own crypto implementations

- Do not use low-level crypto unless you know you have to

- Use the right primitive for the job, and use it right


Background
==========

- Existing libraries: M2Crypto, PyCrypto, PyOpenSSL

- Lack of PyPy, Python 3 support

- Insecure implementations, bad APIs, poor defaults

- Missing some modern primitives, cipher modes

- Lack of high-level APIs for common use cases


Enter *Cryptography*
====================

- Started by Alex Gaynor in July 2013

- *Python Cryptographic Authority* (PyCA) formed Oct 2013

  - Custodian of several crypto libs including *Cryptography*

- Supports Python 2.6, 2.7, 3.3+ and PyPy


Goal
====

- To be Python's *cryptographic standard library*

- Provide high-level *human-friendly* APIs for common use cases

- "Batteries included"

  - c.f. NaCl (http://nacl.cr.yp.to/)

- `Stable API <https://cryptography.io/en/latest/api-stability/>`_


Installation
============

::

  pip install cryptography


Recipes layer
=============

- *Cryptography for humans*

- Safe; few developer decisions needed

- Symmetric encryption, X.509


Fernet
======

- Symmetric encryption recipe

- Implements the Fernet_ specification
  - https://github.com/fernet/spec/blob/master/Spec.md
  - AES-128-CBC with HMAC-SHA256

- Simple key rotation (``MultiFernet``)

.. _Fernet: https://github.com/fernet/spec/blob/master/Spec.md


Fernet - encryption
===================

.. code:: python

    from cryptography.fernet import Fernet

    message = b"for your eyes only"

    # 32-byte base64url-encoded string
    key = Fernet.generate_key()

    f = Fernet(key)

    # base64url-encoded token
    token = f.encrypt(message)


Fernet - decryption
===================

.. code:: python

    # key storage/distribution is out of scope
    f = Fernet(key)

    try:
        message = f.decrypt(ciphertext)

    except cryptography.fernet.InvalidToken:
        print "decryption failed"


X.509
=====

- The prevailing PKI for Internet

- Certificates and CRLs (`RFC 5280`_)

- Certificate requests (PKCS #10, `RFC 2986`_)

- Supports common extensions

.. _RFC 5280: https://tools.ietf.org/html/rfc5280
.. _RFC 2986: https://tools.ietf.org/html/rfc2986


X.509 - certificate request
===========================

.. code:: python

    from cryptography import x509

    cn = x509.NameAttribute(x509.OID_COMMON_NAME, u'foo.com')
    alt_name = x509.DNSName(u'www.foo.com')

    builder = x509.CertificateSigningRequestBuilder() \
        .subject_name(x509.Name([cn])) \
        .add_extension(x509.SubjectAlternativeName([alt_name])

    csr = builder.sign(
        private_key, hashes.SHA256(), default_backend())


Hazmat layer
============

- ``cryptography.hazmat``

- Cryptographic primitives

- Safety off; up to you to use correctly


Primitives
==========

- Digests: SHA-1, SHA-2, ...

- MACs: HMAC

- One-time pads: HOTP, TOTP

- Key-stretching: PBKDF2, HKDF

- Block ciphers: AES, 3DES, ...

- Public-key algorithms: DSA, RSA, ECDSA, DH

- Serialisation: DER, PEM, OpenSSH


Backends
========

- Implementations of primitives are provided by *backends*

- Backends implement *interfaces*
  - ``CipherBackend``, ``HashBackend``, ...

- Backends available: OpenSSL, CommonCrypto (OS X, iOS)

- Use ``MultiBackend`` to compose backends


************
Case studies
************

FreeIPA Vault
=============

- User self-service secret store

- Key escrow

- Symmetric or asymmetric encryption

- http://www.freeipa.org/


FreeIPA Vault
=============

.. code:: python

    def encrypt(self, data,
                symmetric_key=None, public_key=None):

        if symmetric_key:
            return Fernet(symmetric_key).encrypt(data)

        elif public_key:
            return public_key.encrypt(
                data, padding.PKCS1v15())


FreeIPA Vault
=============

.. code:: python

    def decrypt(self, data,
                symmetric_key=None, private_key=None):
        try:
            if symmetric_key:
                return Fernet(symmetric_key).decrypt(data)

            elif private_key:
                return private_key.decrypt(
                    data, padding.PKCS1v15())

        except (InvalidToken, ValueError):
              raise errors.AuthenticationError(
                  message=_('Invalid credentials'))


jwcrypto
========

- Python implementation of JWS / JWE / JWT

- https://github.com/simo5/jwcrypto

- Used by *Custodia*, a secure key distribution service


jwcrypto - imports
==================

.. code:: python

    from cryptography.hazmat.primitives.asymmetric \
      import padding, rsa

    from cryptography.hazmat.primitives import hashes

    padfn = padding.PKCS1v15()
    hashfn = hashes.SHA256()

jwcrypto - read public key
==========================

.. code:: python

    def _rsa_pub(jwk):
        return rsa.RSAPublicNumbers(
            _decode_int(jwk['e']),
            _decode_int(jwk['n'])
        )

jwcrypto - read private key
===========================

.. code:: python

    def _rsa_pri(jwk):
        return rsa.RSAPrivateNumbers(
            _decode_int(jwk['p']),
            _decode_int(jwk['q']),
            _decode_int(jwk['d']),
            _decode_int(jwk['dp']),
            _decode_int(jwk['dq']),
            _decode_int(jwk['qi']),
            _rsa_pub(jwk)
        )


jwcrypto - sign
===============

.. code:: python

    def sign(jwk, payload):
        private_key = _rsa_pri(jwk)

        # get an AsymmetricSignatureContext
        signer = private_key.signer(padfn, hashfn)
        signer.update(payload)

        signature = signer.finalize()
        return signature


jwcrypto - verify
=================

.. code:: python

    def verify(jwk, payload, signature):
        public_key = _rsa_pub(jwk)

        # get an AsymmetricVerificationContext
        verifier = \
            public_key.verifier(signature, padfn, hashfn)
        verifier.update(payload)

        try:
            verifier.verify()
        except cryptography.exception.InvalidSignature:
            # ruh roh


***********
Wrapping up
***********

Security
========

- No memory wiping

- Has not been formally audited

- OpenSSL statically linked on Windows

- Use ``os.urandom`` for randomness


Conclusion
==========

- Avoid low-level crypto where possible

- *Cryptography* has:
  - high-level APIs for common use cases
  - most of the primitives you're ever likely to need

- Consider making it *your* crypto standard library

- If *Cryptography* doesn't meet your needs...
  - are you doing the right thing?
  - contribute!


Resources
=========

Docs:
  https://cryptography.io/

Code:
  https://github.com/pyca/cryptography

Mailing list:
  ``cryptography-dev@python.org``

IRC:
  ``#cryptography-dev`` (Freenode)

Course:
  https://www.crypto101.io/


Fin
===

Copyright 2015  Red Hat, Inc.

This work is licensed under the Creative Commons Attribution 4.0
International License. To view a copy of this license, visit
http://creativecommons.org/licenses/by/4.0/.

Slides
  https://github.com/frasertweedale/talks/
Email
  ``ftweedal@redhat.com``
Twitter
  ``@hackuador``
