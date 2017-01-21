..
  Copyright 2015  Red Hat, Inc.

  This work is licensed under the Creative Commons Attribution 4.0
  International License. To view a copy of this license, visit
  http://creativecommons.org/licenses/by/4.0/.


************
Introduction
************

Identity silos
==============

- Don't build your apps as identity silos

- *python-social-auth* and *allauth* solve this for the open web

- Not all apps are (deployed) for public consumption...


Identity management
===================

- Solutions: FreeIPA, Active Directory, LDAP

- Used by corporations, open-source projects

- Define users, groups, role-based access policies

- Authentication and authorisation services


Single sign-on
===============

- security

- convenience

- onboarding

- avoid duplication of data, administration effort


SSO protocols
=============

- Kerberos
  - Ticket-based authentication protocol
  - Active Directory, MIT Kerberos, Heimdal
  - Browser suppport via HTTP Negotiate (`RFC 4559`_)

- *Security Assertion Markup Language* (SAML)
  - XML format
  - Service provider receives *assertions* containing *attributes*

.. _RFC 4559: https://tools.ietf.org/html/rfc4559


FreeIPA and SSSD
================

- FreeIPA is a centralised IdM

  - Users, groups, services

  - Kerberos *Key Distribution Centre* (KDC)

  - *Host-based Access Control* (HBAC)

- *System Security Services Daemon*

  - PAM responder and user info lookup

  - Enforce access policies defined in FreeIPA or AD

  - DBus interface


****
Demo
****

Demo
====

- Manage identities with FreeIPA
- Kerberos SSO
- Only *django* group can access app (HBAC)
- Load additional user attributes
- Map external groups to app groups
- Let's onboard *Alice*


*************************
Consuming external authnz
*************************

``REMOTE_USER``
===============

- Standard request environment variable to identify remote users

- Web server sets it

- Many apps observe it (yours should, too!)

- In practice ``REMOTE_USER`` is not enough


Server modules (Apache)
=======================

``mod_auth_kerb``
  Kerberos Negotiate support

``mod_authnz_pam``
  Access control via ``pam_sss``

``mod_lookup_identity``
  Populate request environment with user attributes

``mod_intercept_form_submit``
  Intercept credentials and authenticate via PAM

``mod_auth_mellon``
  Handle SAML assertions


Middleware and backend (Django)
===============================

- ``RemoteUserMiddleware`` observes ``REMOTE_USER`` and logs in
  - ``PersistentRemoteUserMiddleware`` for persistent sessions

- ``RemoteUserAttrMiddleware`` reads ``mod_lookup_identity``
  variables and updates user object
  - Not accepted by Django upstream -> 3rd party package

- ``RemoteUserBackend`` creates users by default


Not using Django?
=================

- Use middleware(s) to interpret request environment

- Implement system to map remote groups to app groups / roles

- Users: transient or persisted to app's database?

- Tweak views as needed


Why Apache / why not Python?
============================

- Python makes sense if you *only* deal with Python and need to be
  server-agnostic.

- In heterogeneous environments Apache modules mean:
  - don't have to implement authnz logic in *N* languages
  - apps have less configuration and do less I/O


Resources
=========

- Django ``REMOTE_USER`` auth HOWTO: http://is.gd/y630Yr

- http://www.adelton.com/django/external-authentication-for-django-projects

- FreeIPA *Web App Authentication* wiki page: http://is.gd/w9qZj0

- ``freeipa-users@redhat.com``, ``#freeipa`` on Freenode


Conclusion
==========

- Identity silos -> duplicate data and effort, password fatigue
- If your org has centralised IdM, use it!
- If it doesn't, start planning! Evaluate FreeIPA
- Web server can do the heavy lifting


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


**************
Apache modules
**************

mod_auth_kerb
=============

- Implements Kerberos Negotiate method

- Browser obtains service ticket and transmits to server

- Server verifies ticket

- See also: ``mod_auth_gssapi``

mod_auth_kerb
=============

::

  LoadModule auth_kerb_module modules/mod_auth_kerb.so

  <Location /admin/login/>
    AuthType Kerberos
    AuthName "Kerberos Login"
    KrbMethodNegotiate On
    KrbMethodK5Passwd Off
    Krb5Keytab /etc/http.keytab
    Require valid-user
  </Location>


mod_authnz_pam
==============

- Perform authentication and/or authorisation via PAM

- Works with any module that uses ``Require`` directive

- Can handle password expiry

- Use with ``pam_sss`` to enforce HBAC rules

- Homepage: http://www.adelton.com/apache/mod_authnz_pam/


mod_authnz_pam
==============

Configure PAM stack in ``/etc/pam.d/<service-name>``::

  auth    required  pam_sss.so
  account required  pam_sss.so

Change the ``Require`` directive::

  Require pam-account <service-name>


mod_lookup_identity
===================

- Apps need more than a username

- Looks up user info via SSSD

- Populates request with additional variables

  - ``REMOTE_USER_GROUPS``, ``REMOTE_USER_EMAIL``,
    ``REMOTE_USER_FULLNAME``, ...

  - Full list of proposed variables: http://is.gd/UHcjDH

- Can read *arbitrary* attributes

- Homepage: http://www.adelton.com/apache/mod_lookup_identity/

mod_lookup_identity
===================

::

  LoadModule lookup_identity_module modules/mod_lookup_identity.so

  <Location /admin/login>
    LookupUserAttr email REMOTE_USER_EMAIL " "
    LookupUserAttr firstname REMOTE_USER_FIRSTNAME
    LookupUserAttr lastname REMOTE_USER_LASTNAME
    LookupUserGroupsIter REMOTE_USER_GROUP
  </Location>


mod_intercept_form_submit
=========================

- Authenticate against IdM using normal login form

- Inspects ``POST`` data for user / password fields

- If found, performs PAM authentication (via *mod_authnz_pam*)

- Configure app to trust `REMOTE_USER` and skip its own auth process

- Homepage: http://www.adelton.com/apache/mod_intercept_form_submit/

mod_intercept_form_submit
=========================

::

  LoadModule authnz_pam_module modules/mod_authnz_pam.so
  LoadModule intercept_form_submit_module modules/mod_intercept_form_submit.so

  <Location /admin/login/>
    InterceptFormPAMService django-admin
    InterceptFormLogin username
    InterceptFormPassword password
  </Location>


.. _mod_lookup_identity: http://www.adelton.com/apache/mod_lookup_identity/
.. _mod_intercept_form_submit: http://www.adelton.com/apache/mod_intercept_form_submit/
.. _mod_auth_mellon: https://github.com/UNINETT/mod_auth_mellon
.. _mod_authnz_pam: http://www.adelton.com/apache/mod_authnz_pam/
