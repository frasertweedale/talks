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

- Never used Django until I wrote this talk :)


30,000' view
============

- Stop building your apps as identity silos
- *python-social-auth* solving it for the open web
- Not all apps are for public consumption...


Identity management
===================

- Solutions: FreeIPA, Active Directory, LDAP

- Used by corporations, open-source projects

- Define users, groups, role-based access policies

- Authentication and authorisation services

  - *Identity Provider* (IdP)

  - *Single sign-on* (SSO)


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

  - Kerberos KDC

  - *Host-based Access Control* (HBAC)

- *System Security Services Daemon*

  - PAM responder and user info lookup

  - Enforce access policies defined in FreeIPA or AD

  - DBus interface


****
Demo
****


***********************
External auth in Django
***********************

Server (Apache)
===============

``mod_auth_kerb``
  Kerberos Negotiate support

``mod_authnz_pam``
  Access control via ``pam_sss``

``mod_lookup_identity``
  Populate request environment with user attributes

``mod_intercept_form_submit``
  Intercept credentials and authenticate via PAM


App - configuration
===================

In ``settings.py``:

.. code:: python

  MIDDLEWARE_CLASSES = (
   ...
   'django.contrib.auth.middleware.AuthenticationMiddleware',
   'django.contrib.auth.middleware.PersistentRemoteUserMiddleware',
   'identity.external.RemoteUserAttrMiddleware',
   ...
  )

  AUTHENTICATION_BACKENDS = (
    'django.contrib.auth.backends.RemoteUserBackend',
    'django.contrib.auth.backends.ModelBackend',
  )


App - login form
================

- ``login`` view does not observe ``REMOTE_USER``

  - https://code.djangoproject.com/ticket/25164 (wontfix!)

- Workaround needed

  - Wrap ``django.contrib.auth.views.login``

  - Redirect if ``request.user.is_authenticated()``

  - Update ``urls.py`` to use wrapped view


User creation
=============

- ``RemoteUserBackend`` adds users to database by default

- To disable, subclass and set ``create_unknown_user = False``


User attributes and groups
==========================

- ``RemoteUserAttrMiddleware`` reads ``mod_lookup_identity``
  variables and updates user object

- Proposed, but was rejected wontfix (ticket `#25042`_)

.. _#25042: https://code.djangoproject.com/ticket/25042

- Planning to distribute as 3rd-party package


Resources
=========

- Django ``REMOTE_USER`` auth HOWTO: http://is.gd/y630Yr

- http://www.adelton.com/django/external-authentication-for-django-projects

- FreeIPA *Web App Authentication* wiki page: http://is.gd/w9qZj0

- ``freeipa-users@redhat.com``, ``#freeipa`` on Freenode


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
