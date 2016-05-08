..
  Copyright 2016  Red Hat, Inc.

  This work is licensed under the Creative Commons Attribution 4.0
  International License. To view a copy of this license, visit
  http://creativecommons.org/licenses/by/4.0/.


Title
=====

Enterprise authentication for Python web services


Public abstract
===============

Federated sign-on for the web (e.g. "Sign in with GitHub") is an
increasingly popular means of authenticating users in
publicly-accessible web sites and services - but not all
applications are intended for public access!  In the enterprise, as
on the web, avoiding making your app an identity silo is an
important way to avoid duplication of data, improve administrative
efficiency and reduce security risk.

In this tutorial, attendees will learn how to configure
authentication and authorisation for Python applications hosted
under Apache in a variety of enterprise authentication scenarios,
including:

- Kerberos single sign-on (SSO) via HTTP Negotiate

- SAML-based authentication with ``mod_auth_mellon``

- Authentication and authorisation with arbitrary PAM modules,
  and in particular, FreeIPA host-based access control (HBAC).

- Configuring applications to authenticate to Active Directory,
  FreeIPA or plain LDAP directories via SSSD.

- TLS client certificate authentication (e.g. for use with smart
  cards).

- Populating HTTP requests with additional information about
  authenticated users.

Participants will require Vagrant (with VirtualBox or libvirt
provider).


Private abstract
================

The presentation will begin with a ~15 minute introduction to
identity management concepts and motivations for leveraging existing
user directories and authentication mechanisms within the
enterprise.

Participants will then work through the tutorial curriculum.  Two
virtual machines will be provided with a functional FreeIPA server
and a web server enrolled in the domain.

The curriculum will consist of a number of modules implementing the
various authentication and authorisation scenarios outlined in the
abstract.

For a sample of what some of the curriculum may look like, see
the *Web application authentication* module[1] from the FreeIPA
workshop curriculum used at linux.conf.au 2016.

[1] https://github.com/freeipa/freeipa-workshop#module-5-web-application-authentication-and-authorisation

At intervals, once a significant number of participants have
completed each module, the presenter will complete modules for the
recording and to allow questions to be asked, etc.

The workshop will conclude with an overview of some ongoing work in
the enterprise authentication domain, pointers to additional
resources and a Q&A.

Note: Django apps will be used in the first instance, however, I
will investigate the feasibility of incorporating other frameworks
into the workshop.  Either way, it will be emphasised the concepts
are transferable, even if implementations are not.


Bio
===

Fraser works at Red Hat on the FreeIPA identity management suite and
Dogtag Certificate System.  He cares about security and cryptography
(and making it easier for humans!)  He drank the functional
programming cool-aid and will unabashedly teach Haskell to anyone
who shows an interest - even (especially?) at a Python conference!
