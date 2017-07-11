Q1: MitM, MitB, MotS
====================

- Public and private CAs are **high value targets**

  - Hijack or coerce a CA to issue a fraudulent cert

  - Use fraudulent cert to impersonate a service (MitM or not)

- Customers typically do not deploy Dogtag / IPA publicly accessible

  - Reduced attack surface

- MitB: risk if browser given CA admin/agent creds

  - Unlikely in IPA context

- MotS: low risk


Q2: Access control model
========================

- IPA uses RBAC for controlling access to administrative actions

  - RBAC determines who can issue/revoke certs, add/delete CAs,
    manage CA ACLs, etc.

  - Cert issuance can be **self-service**, based on CA ACLs

- Dogtag uses RBAC

  - Roles are coarse: *Administrator*, *Auditor*, *Agent*

  - IPA possesses a **highly privileged** *Agent* credential


Q3: IP address in Kerberos ticket
=================================

- IPA KDC will give address-restricted ticket *if requested*

- **``kinit -a``** to request; **``klist -a``** to view

- see **``krb5.conf(5)``**: ``extra_addresses``, ``noaddresses``

- see also ``caddr`` in https://tools.ietf.org/html/rfc4120#page-71


Q4: Kerberos secret keys
========================

- Kerberos is based on *symmetric (shared key) encryption*

  - KDC has **all secret keys of all principals** in its
    database

- User's secret key is **derived from passphrase**

- Host/service secret keys are stored in **keytab** files

  - make sure these files have appropriate perms / SELinux
    context


Q5: Requesting service tickets
==============================

- HTTP SPNEGO / Negotiate (`RFC 4559`_)

- When site requires Kerberos auth, **initial request fails** and
  response includes header ``WWW-Authenticate: Negotiate``

- Browser then requests ticket for a service **based on the domain
  name** of the site

- Browser performs the HTTP request again, including header
  ``Authorization: BASE64(TICKET)``

.. _RFC 4559: https://tools.ietf.org/html/rfc4559


Q6: Kerberos vs X.509
=====================

.. raw:: latex

  \begin{tabular}{|l|c|c|c|c|p{0.3\textwidth}|}
  \hline
  ~    & \bf Authn & \bf Priv & \bf Integ & \bf Crypto & \bf Lifetime \\
  \hline
  \hline
  \bf Kerb  & Y     & opt  & opt   & sym    & short (1 day typical) \\
  \hline
  \bf X.509 & Y     & N*   & N*    & asym   & long (2 years typical) \\
  \hline
  \bf TLS   & Y     & Y    & Y     & hybrid & ephemeral (client), X.509 (server) \\
  \hline
  \end{tabular}


Q7: SSSD offline authentication
===============================

- SSSD sees plaintext passphrase
- After successful authentication, store a **hash of the
  passphrase**
- When offline, check credentials against this cache


Q8: LDAP and SQL
================

- Nope.

- Directory servers talk LDAP.

- What is the use case?


Q9: SSSD identity / authentication sources
==========================================

- SSSD can be configured to talk to FreeIPA, AD, bare LDAP

- You *can* configure SSSD to use multiple providers

  - *first match wins*, so you may need to use a qualified name e.g.
    ``fraser@DOMAIN``

.. recent releases allow to consult cache so the fully qualified
   name may be needed only on the first login


Q10: Certmonger and OCSP / CRLs
===============================

- Dogtag CA (therefore FreeIPA) contains an OCSP responder, and
  publishes a CRL

- Certmonger does not know about OCSP/CRL, only expiry
