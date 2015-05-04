..
  Copyright 2015  Fraser Tweedale

  This work is licensed under the Creative Commons Attribution 4.0
  International License. To view a copy of this license, visit
  http://creativecommons.org/licenses/by/4.0/.


Talk Title
==========

Integrating Django with Identity Management Systems


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
  technology like Kerberos to authenticate to applications.

This talk will familiarise the audience with these requirements and
environment and demonstrate how Django applications can be
integrated with an identity management system to meet business
requirements while providing a positive user experience.  Particular
technologies covered will include:

- FreeIPA: an open-source identity management solution, for defining
  users, groups and authorisation policies

- mod_auth_gssapi / mod_auth_kerb: Apache modules for Kerberos
  authentication

- mod_lookup_identity: Apache module to retrieve user information
  from a directory

The talk will conclude with discussion about upcoming Kerberos
features and progress in making identity management integration
easier for Django developers.

People developing or deploying Django applications in business
environments or for large open source projects with centralised
identity management will get the most out of this talk.


Private Abstract
----------------

This talk will demonstrate identity management concepts in action
with a private domain, FreeIPA and an example Django activity app
(all virtual-machine based; network gremlins will not affect the
demo!)


Target audience
===============

Developer - Beginner


Project
=======

FreeIPA

Project homepage
----------------

https://www.freeipa.org/


Biography
=========

Fraser works at Red Hat on the FreeIPA identity management solution
and the Dogtag Certificate System.  He cares about security and
cryptography (and making it easy to use correctly!) and is deeply
interested in functional programming, type theory and theorem
proving.


Relevant Experience
===================

I have spoken regularly at user groups including BrisPy, Brisbane
Functional Programming Group and others.  Previous presentations at
conferences include:

- PyCon AU 2014 (descriptors)
- OSDC 2014 (FreeIPA; Haskell web development)
- linux.conf.au 2015 (FreeIPA; property-based testing)
- DevConf.cz 2015 (property-based testing)
- CrikeyCon 2015 (Let's Encrypt project)
