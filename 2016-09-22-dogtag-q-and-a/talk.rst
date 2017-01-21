..
  Copyright 2016  Red Hat, Inc.

  This work is licensed under the Creative Commons Attribution 4.0
  International License. To view a copy of this license, visit
  http://creativecommons.org/licenses/by/4.0/.


Why do we need NSSDB when cert is stored in LDAP?
-------------------------------------------------

- Applications that use NSS use NSSDB for:
  - Storing trusted CA certs
  - Storing private keys and TLS client or server certs


NSSDBs used in FreeIPA
----------------------

- ``/etc/pki/pki-tomcat/alias``
  - Dogtag

- ``/etc/dirsrv/slapd-<REALM>``
  - 389 DS

- ``/etc/httpd/alias``
  - Apache ``httpd``

- ``/etc/ipa/nssdb``
  - Trust store used by IPA client and utitiles


``/etc/pki/pki-tomcat/alias``
-----------------------------

- ``caSigningCert cert-pki-ca``:
  Main CA signing certificate.

- ``subsystemCert cert-pki-ca``:
  Used by Dogtag CA program to authenticate to LDAP and other
  subsystems.  Issued by main CA.

- ``Server-Cert cert-pki-ca``:
  TLS server certificate for Dogtag website and HTTP APIs.
  Issued by main CA.

- ``auditSigningCert cert-pki-ca``:
  Used to sign audit logs.  Issued by main CA.

- ``ocspSigningCert cert-pki-ca``:
  Used to sign OCSP responses.  Issued by main CA.

May also contain:

- ``caSigningCert cert-pki-ca <UUID>``:
  Lightweight CA certificates (and keys)

- External CA certs:
  When Dogtag CA is signed by external CA


``/etc/httpd/alias``
--------------------

- Used by Apache ``httpd`` and IPA framework

- ``<REALM> IPA CA``:
  IPA CA cert; trust anchor

- ``Server-Cert``:
  IPA web UI / API TLS server cert

- ``ipaCert``:
  RA Agent certificate used by IPA framework to control Dogtag.
  *Plan to eventually remove it and use Kerberos S4U2Proxy
  instead.*

- ``Signing-Cert``:
  Object signing certificate.  Used to sign a Firefox XPI extension
  for browser autoconfig.

- May also contain: external CA certs


What cert verification happens during ipactl start?
---------------------------------------------------

- DS TLS cert and CA subsystem cert verified when Dogtag connects to
  DS

- Apache verifies HTTP TLS cert

- Dogtag verifies certs that it uses

- **Not verified**: *RA Agent* cert
  - will be verified when IPA framework attempts to act upon Dogtag
    e.g. to issue a cert


What are Dogtag subsystem certs; how are they used?
---------------------------------------------------

- Each Dogtag subsystem communicates with DS, possibly other
  subsystems.

- Subsystem certs authenticate Dogtag to DS, other subsystems.

- Dogtag subsystems can be installed on separate hosts, so they use
  different certs by design.


How does certmonger monitor certs?
----------------------------------

- Each *tracking request* indicates location of cert and key
  - for OpenSSL: file paths
  - for NSSDB: path to DB, nickname

- Certmonger daemon *monitors* each cert and triggers renewal when
  expiry is "imminent"
  - "imminent" =< 28 days away


How does certmonger renew certs?
--------------------------------

- Re-issues cert request using same key, extensions, profile and
  target CA as original request.
  - *Does not use dedicated renewal machinery/profile in Dogtag*

- If successful, replaces monitored cert with new cert

- For IPA service certs, uses a *Certmonger CA* (a.k.a. *renewal
  helper*) that invokes ``ipa cert-request`` command
  - these are: IPA HTTP and LDAP certs

- For Dogtag certs, uses Dogtag directly (does not go through IPA
  framework)

- *Post-renewal scripts* may perform additional tasks
  - Update cert in IPA CA cert LDAP tree
  - Update *RA Agent* and Dogtag *subsystem* certs in LDAP
  - Update serial in *Lightweight CA* entry


Solution 643753 - DS/HTTP certs
-------------------------------

- **Q**: Why a different procedure for DS and Apache SSL certs?

- **Context**: DS and Apache TLS certs have corresponding IPA service
  principals and are requested/renewed via ``ipa cert-request``

- **A**: IPA framework is not running when Dogtag certs get renewed.

- **Commentary**: looks like you could ``service httpd restart``
  straight after winding clock back, and things would work.
  - *I have not tested this to confirm*


Solution 643753 - agent cert
----------------------------

- **Q**: Why do we need the help of ``ipaCert`` agent cert to renew
  DS and Apache TLS certs?

- **Context**: DS and Apache TLS certs have corresponding IPA service
  principals and are requested/renewed via ``ipa cert-request``

- **A**: IPA framework uses the ``ipaCert`` agent cert to
  authenticate itself to Dogtag and issue certificates


How does IPA w/ self-signed CA differ from external CA
------------------------------------------------------

- Root and intermediate certs in NSSDBs

- No automatic renewal of IPA CA



Dogtag DIT structure
--------------------

- ``ou=People,{base}`` and ``ou=Groups,{base}``
  - internal account management

- ``cn=aclResources,{base}``
  - internal authz ACLs

- ``ou=requests,{base}``
  - all requests for all subsystems

- ``ou=<subsystem>,{base}``
  - subsystem-specific data; subsystem ∈ {``ca``,``kra``,...}

- ``ou=replica,{base}`` and ``ou=ranges,{base}``
  - replica and range management

- ``ou=Security Domain,{base}``


Dogtag DIT structure (CA subsystem)
-----------------------------------

- ``cn=<seq>,ou=ca,ou=requests,{base}``
  - cert enrolment, renewal and revocation requests

- ``cn=<serial>,ou=certificateRepository,ou=ca,{base}``
  - issued certificates

- ``cn=<uuid>,ou=authorities,ou=ca,{base}``
  - Lightweight CA entries

- ``cn=<id>,ou=certificateProfiles,ou=ca,{base}``


What processes run as part of Dogtag?
-------------------------------------

- **Java** (Tomcat)
  - Whole of PKI runs within this process, except...

- ``/usr/libexec/ipa/ipa-pki-retrieve-key``
  - short-lived helper process for Lightweight CA key replication
  - IPA only (not configured by default)


Logging
-------

- ``/var/log/pki/<instance>/<subsystem>/debug``
  - subsystem debug log; lots of output; no verbosity control

- ``/var/log/pki/<i>/localhost_access_log.<date>.txt``
  - log of all HTTP requests

- ``/var/log/pki/<instance>/catalina.<date>.log``
  - RESTeasy log; routing errors/warnings, etc
  - If request fails but nothing in ``debug`` log, check here

- ``/var/log/pki/<instance>/<subsystem>/transactions``
  - "summarised" subsystem activity

- ``/var/log/pki/<instance>/<subsystem>/transactions``
  - system activity (default level: **Failure**)

- ``/var/log/pki/<i>/<s>/signedAudit/ca_audit``
  - signed audit log


Logging levels
--------------

- Default levels are... fairly verbose
- Change via ``pkiconsole`` or ``CS.cfg``
  - ``log.instance.<instance>.level=<level>``
  - ``instance`` ∈ {``System``, ``Transaction``, ``SignedAudit``}
  - ``level`` ∈ {1 (Info), 2 (Warn), 3 (Failure), 4 (Misconfig), 5
    (Catastrophe), 6 (Security)}


Common workflows
----------------

- Directory authenticated auto-approved cert requests

- Agent-approved cert requests

- Bulk issuance (CMC protocol) e.g. for smart cards
