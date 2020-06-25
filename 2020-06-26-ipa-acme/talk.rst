Objectives
==========

- Understand ACME service design and initial implementation

- Enable code review and QE

- Enable development of next epic

- Suggest future directions


Agenda
======

1. ACME overview
2. Demo
3. Dogtag ACME service overview
4. How FreeIPA deploys and uses Dogtag ACME service
5. How we will test ACME service
6. Current limitations
7. The next epic: deployment-wide configuration; how to approach it.
8. Future work


1: ACME overview
==================

- *Automated Certificate Management Environment*

- Automated *identifer validation*, issuance and renewal

- Automation enables:
  - shorter lifetimes (better security)
  - easier/automatic deployment (→ TLS everywhere)
  - cost savings (less human effort)

- `RFC 8555`_ + others (IETF *acme* working group)

.. _RFC 8555: https://tools.ietf.org/html/rfc8555


1.1: ACME clients and servers
=============================

- *Let's Encrypt*: a major public CA

  - *Boulder*: their CA software

- *Certbot*: popular general-purpose client
  - in Fedora, not in RHEL

- *mod_md*: Apache httpd client module
  - in Fedora and RHEL

- Many other client programs, libs and plugins


2. Demo
=======


3. Dogtag ACME service
======================

- Optional subsystem; Tomcat application in ``pki-tomcatd`` process

- Acts as *registration authority (RA)* to some issuer

- Supports different issuer systems e.g. Dogtag, NSSDB

- Supports different databases e.g. LDAP, PostgreSQL

- Supports ``dns`` identifer and ``http-01`` and ``dns-01``
  challenges

- File-based configuration


4. FreeIPA ACME service
=======================

- Deploy Dogtag ACME service with Dogtag issuer

- Store objects in Dogtag LDAP (``ou=acme,o=ipaca``)

- ACME accounts are unrelated to FreeIPA accounts

- Deploy automatically on **all CA servers** (initially disabled)

- Point clients at ``https://ipa-ca.$DOMAIN/acme/directory``

- Upstream design doc: https://www.freeipa.org/page/V4/ACME


4.1: Supported clients
======================

- ACME service is client-agnostic

- It *should* work if client...

  1. asks for supported identifier type (``dns``)

  2. can respond to an offered challenge types (``http-01``,
     ``dns-01``)

- Impossible to test all clients

  - Can strengthen support statement for those we do test

- Clients could have bugs/quirks (probably server too).  As we roll
  out, we will find out.


4.2: Pull request
=================

- https://github.com/freeipa/freeipa/pull/4723

- This is what was planned for RHEL 8.3, but didn't make it

- It is ready for review

- ``master`` only
  - backport to ``ipa-4-8`` is feasible if required



5. Testing
==========

- integration tests in ``test_integration/test_acme.py``

  - added to gating, for now

- enable/disable checks (via *curl*)

- *Certbot*: http-01, dns-01 and revocation
  - Fedora only; no Certbot on RHEL

- *mod_md*: http-01
  - Fedora + RHEL; dns-01 is possible


5.1: Test enablement
========================

- ``/usr/libexec/ipa/acme/certbot-dns-ipa``
  - hook script for Certbot ``dns-01`` challenge
  - in ``freeipa-client`` package
  - might be useful for users/customers

- similar program could be written for *mod_md* dns-01



6.1. Limitations (features)
===========================

- Only ``dns`` identifier

- Only ``http-01`` and ``dns-01`` challenges

- Cannot configure challenge types to use

- Issuer is ``ipa`` CA (cannot use sub-CA)

- No account key rollover

- *All of these* require work on Dogtag side


6.2. Limitations (administration)
=================================

- Configuration is file-based and per-server

- Cannot specify a different profile (but you can modify that
  profile)

- Cannot direct requests to different profiles

  - This would be required if we ever support different identifier
    types


7: The next epic
==================

FreeIPA-4669: *Manage the ACME service topology-wide from single
system.*

- Consistent with rest of FreeIPA admin experience

- Easier for admin: config change gets replicated

- Prevent inconsistent configuration (which is dangerous)


7.1: FreeIPA parts
==================

- IPA API commands to manage ACME, e.g. (not prescriptive):

  - ``ipa acme-config-mod --enabled=1``
  - ``ipa acme-config-show``

- Associated permissions

- Deprecate or remove ``ipa-acme-manage`` command, or wrap new
  commands.


7.2: Dogtag parts
=================

- LDAP *config source*: monitor or poll ACME config entry

- If config object is in FreeIPA subtree, may need GSS-API authn

  - ``dogtag/$SERVER_FQDN@$REALM`` principal already exists

  - State of GSS-API bind support in *ldapjdk* is unknown

- Otherwise maybe find a way to bind as
  ``uid=pkidbuser,ou=people,o=ipaca``, or write the config object
  under ``o=ipaca``.

7.3: Suggested approach
=======================

1. Assign FreeIPA developer + Dogtag developer

2. Preliminary investigation of ldapjdk GSS-API support,
   cross-database authentication

3. Agree on LDAP config object schema

4. Most work can be done in parallel


8.1. Future work: SHOULD
========================

- Configure (sub-)CA to use for ACME

- Configure challenges

- Expired object pruning (RHCS-798)

- ``tls-alpn-01`` challenge

- Performance testing (RHCS-1259)


8.2. Future work: MAYBE
=======================

- ``ip`` identifier

- Configure profile

- Block/allow lists (RHCS-1276)


8.3. Future work: MAYBE NOT
===========================

- Allow wildcard certificates

  - Customers will probably ask, we should push back; automation
    subsumes wildcard certs

8.4. Future work: SPECULATION
=============================

- `End-user`_ and `S/MIME`_ certs?

.. _End-user: https://datatracker.ietf.org/doc/draft-ietf-acme-client/
.. _S/MIME: https://datatracker.ietf.org/doc/draft-ietf-acme-email-smime/

- Integration with IDM accounts?

  - "Enterprise ACME" is totally undefined; let's listen to
    customers and keep an open mind

Questions
=========
