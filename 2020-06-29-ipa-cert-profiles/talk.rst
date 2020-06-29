Objectives
==========

- Understand Dogtag profile configuration

- Understand FreeIPA-specific profile configuration

- Support enablement: analysing profile-related issues


Agenda
======

1. What are certificate profiles?
2. Dogtag profile configuration
3. Dogtag profile management
4. FreeIPA-specific profile configuration
5. FreeIPA *included profiles*
6. FreeIPA profile management (``certprofile`` plugin)
7. Common support scenarios



1: What are certificate profiles?
=================================

- Define how to construct a certificate given CSR and request
  environment.

- Some aspects usually static (e.g. validity period, key usage)

- Some aspects usually dynamic (e.g. Subject DN, SAN)

- All enterprise CAs implement this concept
  - AD-CS calls them *templates*


2. Dogtag profile configuration
===============================

There are two types of profiles:

- *enrol* profiles

  - Given CSR, issue certificate according to profile

- *renewal* profiles

  - Given serial, issue new cert according to profile used
    for previous cert

  - Replication issues, range conflicts, deleted data → brittle!

  - Previously used for FreeIPA system cert renewal, but not anymore
    (`#7991`_)

.. _#7991: https://pagure.io/freeipa/issue/7991


2.2: Anatomy of profile configuration
=====================================

- Doc link: `RHCS profile config parameters`_

- type/class (type; **enrol**/renewal)

- **Profile ID**, name, descrption

- *visible* and *enabled* flags

- Optional authn/authz plugins
  - e.g. restrict use of profile to a particular group

- Inputs (**CSR**, submitter info, ...)

- Outputs (**certificate**, PKCS 7, ...)

- One or more *policy sets* (almost always 1)

.. _RHCS profile config parameters: https://access.redhat.com/documentation/en-us/red_hat_certificate_system/9/html/administration_guide/Setting_up_Certificate_Profiles#Profile_Configuration_Parameters-Profile_Configuration_File_Parameters

2.2.1: Example
==============

::

  classId=caEnrollImpl
  profileId=caIPAserviceCert
  name=IPA Service Certificate
  desc=Enrol server cert with IPA-RA agent authn
  auth.instance_id=raCertAuth
  input.list=i1,i2
  input.i1.class_id=certReqInputImpl
  input.i2.class_id=submitterInfoInputImpl
  output.list=o1
  output.o1.class_id=certOutputImpl
  policyset.list=x
  policyset.x.list=1,2,3,4,5,6,7,8,9,10,11
  policyset.x.1.constraint.class_id=fooConstraintImpl
  policyset.x.1.constraint.params.p1=value
  policyset.x.1.default.class_id=fooDefaultImpl
  policyset.x.1.default.params.p1=value
  ...

2.3: Policies
=============

- *Policy set* has a list of policy objects
- Policy objects define how to build the cert
- Every index must have a *constraint* and *default* object
  - *default*: set fields/extensions
  - *constraint*: ensure values meet requirements
- Each field/extension has corresponding component(s)
- There is a *noop* constraint for static defaults

2.3.1: Example: Subject DN
==========================

*(note:* ``$`` *denotes substitution;* ``$$`` *is an escape; lines
wrapped for display)*

::

  policyset.x.n.constraint.class_id=subjectNameConstraintImpl
  policyset.x.n.constraint.name=Subject Name Constraint
  policyset.x.n.constraint.params.pattern=CN=[^,]+,.+
  policyset.x.n.constraint.params.accept=true
  policyset.x.n.default.class_id=subjectNameDefaultImpl
  policyset.x.n.default.name=Subject Name Default
  policyset.x.n.default.params.name=
    CN=$$request.req_subject_name.cn$$, $SUBJECT_DN_O

2.3.2: Example: Extended Key Usage (EKU)
========================================

::

  policyset.x.n.constraint.class_id=noConstraintImpl
  policyset.x.n.constraint.name=No Constraint
  policyset.x.n.default.class_id=extendedKeyUsageExtDefaultImpl
  policyset.x.n.default.name=Extended Key Usage Extension Default
  policyset.x.n.default.params.exKeyUsageCritical=false
  policyset.x.n.default.params.exKeyUsageOIDs=
    1.3.6.1.5.5.7.3.1,1.3.6.1.5.5.7.3.2


3. Dogtag profile management
============================

- ``ProfileSubsystem`` (local files) or ``LDAPProfileSubsystem``

- Common Criteria requires separate permissions for "defining" and
  "approving" profiles.

  - new profile: import, then enable
  - modify profile: disable, update, re-enable
  - FreeIPA ``certprofile`` plugin handles this transparently

- FreeIPA uses ``LDAPProfileSubsystem`` (since v4.2)


3.1. REST API
=============

- List: ``GET /ca/rest/profiles``

- Create: ``POST /ca/rest/profiles``

- Show: ``GET /ca/rest/profiles/{id}[/raw]``

- Modify: ``PUT /ca/rest/profiles/{id}[/raw]``

- Enable/disable: ``POST /ca/rest/profiles/{id}?action={enable,disable}``

- Delete: ``DELETE /ca/rest/profiles/{id}``

3.2. ``LDAPProfileSubsystem``
=============================

- Each profile is an object under
  ``ou=certificateProfiles,ou=ca,{dogtagBaseDN}``

- Persistent search to observe add/delete/modify

- Uses ``entryUSN`` and ``nsUniqueId`` to avoid redundant work
  and handle replication races


3.2.1. Dogtag profile object
============================

.. code:: ldif

  dn: cn=caIPAserviceCert,ou=certificateProfiles,ou=ca,o=ipaca
  objectClass: top
  objectClass: certProfile
  cn: caIPAserviceCert
  classId: caEnrollImpl
  certProfileConfig:: YXV0... # profile config (base64)


4. FreeIPA-specific profile configuration
=========================================

::

  # Allow members of "Registration Manager Agents" group (required)
  auth.instance_id=raCertAuth

  # Put subject base in Subject DN
  policyset.x.n.default.params.name=
    CN=$$request.req_subject_name.cn$$, $SUBJECT_DN_O

  # Set OCSP responder URL
  policyset.x.n.default.params.authInfoAccessADLocation_0=
    http://$IPA_CA_RECORD.$DOMAIN/ca/ocsp

  # Set CRL URL
  policyset.x.n.default.params.crlDistPointsPointName_0=
    http://$IPA_CA_RECORD.$DOMAIN/ipa/crl/MasterCRL.bin


5. Included profiles
====================

- ``caIPAserviceCert``: default profile, suitable for TLS servers

- ``IECUserRoles``: obscure "smart grid" use case;
  initial driver of custom profiles

- ``KDCs_PKINIT_Certs``: KDC cert (used internally)

- ``acmeServerCert``: [soon] ACME profile

  - uses different auth plugin; see `V4/ACME#Profile`_

.. _V4/ACME#Profile: https://www.freeipa.org/page/V4/ACME#Profile

5.1. Adding an included profile
===============================

- Add to ``install/share/profiles.cfg``
- Update ``install/share/profiles/Makefile.am``
- Update ``INCLUDED_PROFILES`` dict in ``ipapython/dogtag.py``


5.2. Updating included profile
==============================

- If a backwards compatible change (e.g. maximum key size), go ahead

  - New deployments will use it.  Existing deployments have to be
    manually updated.

- If not backwards compatible (e.g. uses new profile component), it
  is probably still OK to update

  - ensure RPM spec ``Requires:`` correct version of Dogtag

  - **AND** upgrade scripts add the new components to existing installs

  - If you don't... `#7097`_

- We need a proper framework for updating profiles in existing
  deployments (`#5323`_)

  - esp. for the IPA→Dogtag GSS-API effort (`#5011`_)

.. _#7097: https://pagure.io/freeipa/issue/7097
.. _#5323: https://pagure.io/freeipa/issue/5323
.. _#5011: https://pagure.io/freeipa/issue/5011


6. ``certprofile`` plugin
==========================

- Commands and associated permissions for modifying profiles

  - ``certprofile-{find,import,show,mod,del}``

  - ``certprofile-mod``: changes are replicated and applied
    immediately

- ``ipaserver/plugins/certprofile.py``

- Uses IPA RA certificate to authenticate to Dogtag REST API

- FreeIPA-specific configuration:
  - ``ipaCertProfileStoreIssued`` / ``--store``


6.1. FreeIPA profile object
===========================

.. code:: ldif

  dn: cn=caIPAserviceCert,cn=certprofiles,cn=ca,dc=ipa,dc=local
  objectClass: ipacertprofile
  objectClass: top
  cn: caIPAserviceCert  #   = Dogtag profile ID
  description: Standard profile for network services
  ipacertprofilestoreissued: TRUE



7. Common support scenarios
===========================

Dogtag hangs on startup
=======================

- Unexpected entries or conflict entries under
  ``ou=certificateProfiles`` caused startup to hang (`BZ 1638379`_)

- Fixed in ``pki-core-10.5.16-3.el7`` (7.7.z) and ``10.7.0-1`` (8.1)

- Cases still occur; upgrade to version with fix

.. _BZ 1638379: https://bugzilla.redhat.com/show_bug.cgi?id=1638379

"I need a new profile for X"
=================================

- Export an existing configuration

  - ``ipa certprofile-show caIPAserviceCert --out newprofile.cfg``

- Modify ``newprofile.cfg`` as required

  - `RHCS profile policy component doc`_

- Import as a new profile

  - ``ipa certprofile-import newprofile --file newprofile.cfg --store=1``

.. _RHCS profile policy component doc: https://access.redhat.com/documentation/en-us/red_hat_certificate_system/9/html/administration_guide/certificate_and_crl_extensions


New or modified profile rejected
================================

- IPA command output *may* have useful information

- If not, inspect Dogtag debug log

- Policy component missing constraint object

- Default or constraint missing required param

- Check against known-good configs that use the param

- Ask a Dogtag developer or "Use the Source"


Profile config accepted, but issuance fails
===========================================

- Missing values manifest at import/modify; *bad values*
  often manifest during issuance

- Initial diagnostic steps are otherwise the same as previous slide


Change validity period (lifetime)
=================================

::

  policyset.serverCertSet.2.default.class_id=validityDefaultImpl
  policyset.serverCertSet.2.default.name=Validity Default
  policyset.serverCertSet.2.default.params.range=90
  policyset.serverCertSet.2.default.params.startTime=0

- default unit is days
- can specify ``rangeUnit={year,month,day,hour,minute}``



Basic Constraints extension
===========================

Some customers want to add *Basic Constraints* extension even for
end-entity (a.k.a. leaf; non-CA) certs.

::

  policyset.x.n.default.class_id=basicConstraintsExtDefaultImpl
  policyset.x.n.default.name=Basic Constraints Extension Default
  policyset.x.n.default.params.basicConstraintsCritical=true
  policyset.x.n.default.params.basicConstraintsIsCA=false
  policyset.x.n.default.params.basicConstraintsPathLen=0
  policyset.x.n.constraint.class_id=noConstraintImpl
  policyset.x.n.constraint.name=No Constraint



Certificate Policies extension
==============================

Some customers want to add Certificate Policies extension to the
certs they issue.

::

  policyset.x.n.default.class_id=certificatePoliciesExtDefaultImpl
  policyset.x.n.default.name=Certificate Policies Extension Default
  policyset.x.n.default.params.PolicyQualifiers.num=0
  policyset.x.n.default.params.PoliciesExt.num=1
  policyset.x.n.default.params.PoliciesExt.certPolicy0.enable=true
  policyset.x.n.default.params.PoliciesExt.certPolicy0.policyId=
    2.16.840.1.101.3.2.1.2.13
  policyset.x.n.constraint.class_id=noConstraintImpl
  policyset.x.n.constraint.name=No Constraint


Customising the Subject DN
===============================

- e.g. add an OU to the Subject DN

- Can be statically configured

  - Define a new profile and use CA ACLs to restrict use to target
    group

- Dynamic options limited; see `dogtagpki#3012`_

  - I will do a blog post...

.. _dogtagpki#3012: https://pagure.io/dogtagpki/issue/3012


Resources
=========

- `RFC 5280`_: understand extensions, use cases, criticality

- RHCS documentation:
  - `RHCS profile config parameters`_
  - `RHCS profile policy component doc`_

- Dogtag design doc: `LDAP Profile Storage`_
- Upstream design doc: `V4/Certificate Profiles`_

- My blog: `posts with "profiles" tag`_


.. _RFC 5280: https://tools.ietf.org/html/rfc5280
.. _LDAP Profile Storage: https://www.dogtagpki.org/wiki/LDAP_Profile_Storage
.. _V4/Certificate Profiles: https://www.freeipa.org/page/V4/Certificate_Profiles
.. _posts with "profiles" tag: https://frasertweedale.github.io/blog-redhat/tags/profiles.html


Questions
=========
