..
  Copyright 2015  Fraser Tweedale

  This work is licensed under the Creative Commons Attribution 4.0
  International License. To view a copy of this license, visit
  http://creativecommons.org/licenses/by/4.0/.


Talk Title
==========

Integrating Python Apps with Centralised Identity Management Systems


Brief Outline
=============

Centralised identity management is common in businesses and large
open-source projects.  Users, groups, service identities and access
policies are stored centrally rather than in individual
applications' database tables.  This talk covers integrating Django
applications into these environments to achieve organisational
security objectives while providing a positive user experience.


Abstract
========

Most Python web developers are familiar with authentication and
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
demonstrate how Python web applications can be integrated with an
identity management system to meet business requirements while
providing a positive user experience.  Particular technologies
covered will include:

- [FreeIPA](https://www.freeipa.org): an open-source identity
  management solution, for defining users, groups and authorisation
  policies

- [mod_auth_gssapi](https://github.com/modauthgssapi/mod_auth_gssapi) /
  mod_auth_kerb: Apache modules for Kerberos authentication

- [mod_lookup_identity](http://www.adelton.com/apache/mod_lookup_identity/): Apache
  module to retrieve user information from a directory

The talk will conclude with discussion about upcoming Kerberos
features, techniques for dealing with multiple authentication
methods, and progress in making identity management integration
easier for Python (and in particular, Django) developers.

People developing or deploying Python web applications in business
environments or for large open source projects with centralised
identity management will get the most out of this talk.


Biography
=========

Fraser works at Red Hat on the FreeIPA identity management solution
and the Dogtag Certificate System.  He cares about security and
cryptography (and making it easier for humans!) and is deeply
interested in functional programming, type theory and theorem
proving.
