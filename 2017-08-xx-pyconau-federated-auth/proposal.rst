..
  Copyright 2017  Fraser Tweedale

  This work is licensed under the Creative Commons Attribution 4.0
  International License. To view a copy of this license, visit
  http://creativecommons.org/licenses/by/4.0/.


Talk Title
==========

Identity 2.0: the what, why and how of social and federated login


Length
======

70-minute extended session


Abstract
========

Whether you're developing public-facing web apps or deploying behind
the corporate firewall, the days of identity silos are over.  Social
auth (*log in with FooBookHub*) and federated identity (*SAML*,
*OpenID Connect* and friends) are the new normal.  The advantages
are clear: developers and operators have less security-sensitive
code to write and deploy, and users experience less password/account
fatigue, and improved productivity through *single sign-on*.

The wins are clear, but like most things in technology there is a
trade-off.  Federated authentication protocols are inherently more
elaborate than plain old passwords; more moving parts means more
complex deployment and more points of failure.

In this extended session for web developers and
administrators/operations folks, attendees will learn and experience
how to deploy and use federated auth, end-to-end from the identity
provider to the app.  The session will cover:

- The basics of federated authentication including **protocol**
  overviews and comparisons.

- How to use **social auth** providers for public-facing
  applications, allowing users to log in with an account they
  already have.

- How to leverage accounts in centralised identity management
  systems (*FreeIPA*, *Active Directory*, *LDAP*, etc) for **single
  sign-on** in an organisation.

- How **identity brokers** like *Keycloak* make it easy to use a
  variety of external authentication providers, and provide a
  consistent user experience across multiple applications.

- How to use external identities in your applications with the help
  of your web server, focusing in particlar on popular **Python web
  frameworks** and Apache (though the principles are more widely
  applicable).

- **Security** characteristics, and discussion of some challenging
  scenarios including testing, account merging and single sign-out.



Private Abstract
----------------

The first 15 minutes or so will comprise the overview of federated
auth, SSO, protocols etc.  From there the demos begin, starting with
setting up a Keycloak instance (probably on an OpenShift deployment
somewhere, or locally if network availability is dicey) and a simple
example application.  I will hook keycloak up to one or more social
auth providers (probably GitHub and one other) and demonstrate
login.  Then I will connect Keycloak to a (pre-baked) FreeIPA
deployment to demonstrate the "enterprise" side of things.

Next I will demonstrate and discuss how to use Apache modules to do
the heavy lifting in consuming data from Keycloak, and how to
configure Python web frameworks to observe externally authenticated
identities.

The session concludes with the discussion of security, and
unique challenges in federated identity / SSO.

Questions will be encouraged during the demonstrations and at the
end of the session.

The demo will focus on Apache, and hopefully a handful of Python web
frameworks (but Django at the very least).  The principles and other
technologies including Keycloak are applicable to web development
and deployment more broadly, regardless of programming language,
framework, or server software.


Target audience
===============

Developer - Intermediate


Project
=======

Keycloak

Project homepage
----------------

https://www.keycloak.org/


Biography
=========

Fraser works at Red Hat on the FreeIPA identity management system
and the Dogtag Certificate System.  He's interested in security,
cryptography, functional programming, type theory and theorem
proving.  Sometimes writes Python.  Crazy about jalapeños.


Relevant Experience
===================

I present at Brisbane users groups including BrisPy and Brisbane
Functional Programming Group and have presented talks or workshops
at conferences including:

- PyCon AU 2014, 2015
- linux.conf.au 2015, 2016, 2017
- OSDC 2014, 2015
- DevConf.cz 2015
- CrikeyCon 2015, 2016, 2017
- AusCERT 2016
- YOW! Lambda Jam 2015, 2016, 2017
