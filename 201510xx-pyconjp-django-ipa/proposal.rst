..
  Copyright 2015  Fraser Tweedale

  This work is licensed under the Creative Commons Attribution 4.0
  International License. To view a copy of this license, visit
  http://creativecommons.org/licenses/by/4.0/.


Talk Title
==========

Integrating Django with Centralised Identity Management

Category: Business

Language: English


Description
===========

Centralised identity management is common in businesses and large
open-source projects.  Users, groups, service identities and access
policies are stored centrally rather than in individual
applications' database tables.  This talk covers integrating Django
applications into these environments to achieve organisational
security objectives while providing a positive user experience.


Audience
========

People developing or deploying Django applications in large
organisations.

Level: Novice


Objectives
==========

Attendees will learn high-level details of centralised identity
management systems and Kerberos, and how they are used by
organisations.  They will see how to integrate Django applications
with the FreeIPA identity management system using Kerberos for
authentication and enforcing access policies.


Abstract
========

Most Django developers are familiar with authentication and
authorisation on the open web, but the requirements and technologies
used inside companies and large organisations are different:

- Identities and groups are probably stored in an external identity
  management system's directory rather than in an application's
  database tables.

- Authorisation decisions will be based on group membership and
  policies that are defined outside the application.

- Users may be expected or required to use a *single sign-on*
  technology such as Kerberos or SAML to authenticate to
  applications.

This talk will familiarise the audience with these technologies and
demonstrate how Django applications can be integrated with an
identity management system to meet business requirements while
providing a positive user experience.  Particular technologies
covered will include:

- FreeIPA: an open-source identity management solution, for defining
  users, groups and authorisation policies

- mod_auth_gssapi / mod_auth_kerb: Apache modules for Kerberos
  authentication

- mod_lookup_identity: Apache module to retrieve user information
  from a directory

The talk will conclude with discussion about upcoming Kerberos
features, techniques for dealing with multiple authentication
methods, and progress in making identity management integration
easier for Django developers.

People developing or deploying Django applications in business
environments or for large open source projects with centralised
identity management will get the most out of this talk.


Outline
=======

1. Explanation of need for centralised identity management in large
   organisations (central identity and policy store, SSO to reduce
   password-related risk)

2. Overview of identity management technologies (LDAP, Kerberos,
   SAML, certificates, OTP).

3. Detailed examination of Django integration with FreeIPA
   (mod_auth_kerb, mod_lookup_identity, user auto-creation, groups
   and authorisation) including demo.

4. Discussion of relevant upcoming Kerberos features and progress in
   making integration simple for all Django developers.


Additional Notes
================

I have spoken regularly at user groups including BrisPy, Brisbane
Functional Programming Group and others.  Previous presentations at
conferences include:

- PyCon AU 2014 (descriptors)
- OSDC 2014 (FreeIPA; Haskell web development)
- linux.conf.au 2015 (FreeIPA; property-based testing)
- DevConf.cz 2015 (property-based testing)
- CrikeyCon 2015 (Let's Encrypt project)
