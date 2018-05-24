..
  Copyright 2018  Red Hat, Inc.

  This work is licensed under the Creative Commons Attribution 4.0
  International License. To view a copy of this license, visit
  http://creativecommons.org/licenses/by/4.0/.


Title
=====

FreeIPA: Open Source Identity Management


Elevator pitch
==============

In this workshop participants will learn about FreeIPA, an Open
Source identity management system.  Attendees will get hands-on
experience installing and managing FreeIPA, with a focus on
leveraging platform and federated identity services for web
applications.


Abstract
========

FreeIPA is an integrated identity management solution providing
centralised user, host and service management, authentication and
authorisation in Linux/UNIX networked environments, with a focus on
ease of deployment and management.  It is built on top of well-known
Open Source technologies and standards including 389 Directory
Server, MIT Kerberos and Dogtag Certificate System.

Organisations use FreeIPA to provide centralised management of
identities and security policies.  By avoiding identity silos,
security goals can be achieved with reduced effort and improved
auditability.  Web applications can be configured to hook into
identity management systems, either directly or via *federation*
protocols like SAML or OpenID Connect.

This session will begin with a short presentation providing an
overview of FreeIPA's capabilities and architecture.  Web
authentication protocols will be discussed in brief.  The rest of
the session is devoted to the practical workshop curriculum.
Participants will:

- Install a FreeIPA server and replica and enrol a client machine

- Create and manage users and access control policies

- Issue a TLS certificate for an HTTP server

- Configure a web server to use FreeIPA for user authentication and
  access control

There are some elective units participants can choose from, based on
their progress and interests:

- OTP two-factor authentication

- Advanced certificate management: profiles, sub-CAs and user
  certificates

- Configuring Keycloak to authenticate against FreeIPA and
  configuring the web server to process SAML/OpenID Connect
  assertions.

Project URL: http://www.freeipa.org/

To get the most out of the workshop, participants should complete
the preparation steps beforehand:
https://github.com/freeipa/freeipa-workshop#preparation


Bio
===

Fraser works at Red Hat on the FreeIPA identity management system
and Dogtag Certificate System. He's interested in security,
cryptography and functional programming.  Jalapeño aficionado from
Down Under.
