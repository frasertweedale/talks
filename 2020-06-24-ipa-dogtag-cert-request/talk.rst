IPA-CS fundamentals and cert-request
====================================

- FreeIPA needs to talk to Dogtag a.k.a. *Certificate System (CS)*
  for most certificate-related actions

- FreeIPA commands execute with privileges of authenticated
  FreeIPA principal (*operator*)

- Dogtag has its own user accounts, permissions, etc.

- FreeIPA permissions (and additional logic) authorises use of
  *IPA RA* credential which maps to privileged Dogtag account.


What is an RA?
==============

- *Registration Authority*

- RA authorises certificate requests and passes them to CA.

- CA just obeys.


IPA RA permissions
==================

- Issue certificates

- Revoke certificates

- Manage profiles and lightweight CAs


IPA RA credential
=================

- ``/var/lib/ipa/ra-agent.{key,pem}`` (NSSDB in older releases)

- shared across replicas

- renewed by renewal master and distributed over LDAP
  - renewal is covered in other presentation(s)


Privilege separation
====================

- Latent IPA RA credential is a security risk.

- Incorrect authz checks already led to several CVEs

- Goal: IPA authenticates to Dogtag as *operator* using proxy
  credential, Dogtag authorises request using logic supplied by
  FreeIPA

- Design: https://www.freeipa.org/page/V4/Dogtag_GSS-API_Authentication

- Big effort, complex migration, stalled for a long time.


Communication and authentication
================================

- Dogtag has mix of REST and traditional servlet (XML payloads)

- Low-level interface in ``ipapython/dogtag.py``

  - Client cert authentication, headers, payload decoding

- High-level interface in ``ipaserver/plugins/dogtag.py``

  - ``request_certificate()``, ``revoke_certificate()``, etc



cert-request
============

- Because IPA is RA, IPA has a lot to check!

- One of the most complex commands


cert-request: concepts
======================

- **operator principal**: The principal executing the command

- **subject principal**: The principal for whom the cert is being
  requested

- **profile**: The Dogtag certificate profile to be used

- **ca**: The (lightweight) CA to use to issue the certificate


cert-request: stages
====================

#. Operator authorisation: does the *operator* have permission to
   request the certificate on the *subject*'s behalf

#. CA ACL check: is the combinator of *subject principal*, *profile*
   and *ca* valid?

   - e.g. restrict server cert profile to webserver hostgroup

#. Check CSR matches *subject principal*

#. Issue certificate

#. Add certificate to *subject principal's* ``userCertificate``
   attribute (profile can suppress)


cert-request: operator authorisation
====================================

Two conditions must be satisfied:

1. self-service OR *operator* is ``host/`` principal OR *operator*
   has ``request certificate`` privilege

2. *operator* can write ``userCertificate`` attribute of *subject
   principal*

   - self-service, host/service ``managedBy`` relationship


cert-request: CA ACL check
==========================

- *operator* has ``request certificate ignore caacl`` → skip

- CA ACLs are defined and evaluated like HBAC rules

  - can *subject* "access" (use) the "service" (profile) on the
    target "host" (CA)

  - we actually (ab)use *libipahbac* / ``pyhbac``!

- default install has one caacl: ``hosts_services_caIPAserviceCert``


cert-request: CSR check
=======================

- Subject DN (CN) and all SANs must match subject principal(s)

- Supported SAN types: dns, rfc822 (email), ipAddress,
  KRB5PrincipalName, UPN

- Other SAN types → reject request


cert-request: CSR check: Subject DN
===================================

- user

  - Common Name (CN) match ``uid``

  - Email Address (emailAddress) match ``mail``

- host/service

  - CN: match FQDN of *subject* OR other host/service principal to
    which *operator* has write access to ``userCertificate``
    attribute.

- Other attributes ignored


cert-request: CSR check: SAN dnsName
====================================

- host/service: match FQDN of *subject* OR other host/service
  principal to which *operator* has write access to
  ``userCertificate`` attribute.

- user: reject request


cert-request: CSR check: SAN rfc822Name
========================================

- user: match ``mail``

- host/service: reject request


cert-request: CSR check: SAN KRB5PrincipalName / UPN
====================================================

- match against principal name/aliases (all types)


cert-request: CSR check: SAN ipAddress
=======================================

- user: reject request

- host/service: check against IPA DNS

  - see blog post for details:
    https://frasertweedale.github.io/blog-redhat/posts/2019-02-18-freeipa-san-ip.html


cert-request: other notes
=========================

- ``cert-request`` can **create the subject principal**

  - ``add=True`` AND *subject* is a (non-host) service princpal
    AND *operator* has permission to add

- Several special cases for KDC certificates.



cert-request: open tickets
==========================

- `#8087`_ Implicit CA ACL for IPA services

- `#6424`_ Extend CA ACLs to encompass operator authorisation

- (not exhaustive!)

.. _#8087: https://pagure.io/freeipa/issue/8087
.. _#6424: https://pagure.io/freeipa/issue/6424


Topics for upcoming session
===========================

- Profile management

- Lightweight (sub-)CAs

- Revocation

- ACME

- KRA (Vault)?



Questions?
==========
