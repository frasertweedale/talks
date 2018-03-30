..
  Copyright 2018  Fraser Tweedale

  This work is licensed under the Creative Commons Attribution 4.0
  International License. To view a copy of this license, visit
  http://creativecommons.org/licenses/by/4.0/.


Talk Title
==========

Identity 2.0: the what, why and how of social and federated login


Elevator pitch
==============

Are you sick of managing so many online accounts? Password fatigue
got you down? If you're a service provider, how can you avoid
contributing to the problem?

Federated/social login is the solution. In this talk you will learn
what it is, how it works, and why you should care. And see it in
action.


Length
======

45-minute talk


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

In this session for web developers and administrators, attendees
will learn and experience how to deploy and use federated auth,
end-to-end from the identity provider to the app.  The session will
cover:

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

The presentation will include a demo of a sample application being
configured to use social login, using Keycloak as an identity
broker.


Notes
=====

45-minute session.

The first 10 or 15 minutes will provide an overview of federated
auth, SSO, protocols etc.  Next comes the demo, where a simple
Django app with an Apache reverse proxy will be configured for
GitHub social login using Keycloak to broker the authentication.
Principles and technologies are applicable to other frameworks,
languages and web servers.

Next I will discuss how to use web server modules to do the heavy
lifting in consuming identity assertions, and how to configure
Python web frameworks to observe externally authenticated
identities.

The session concludes with a discussion of security, and unique
challenges in federated identity / SSO.


Bio
===

Fraser works at Red Hat on the FreeIPA identity management system
and Dogtag Certificate System. He's interested in security,
cryptography and functional programming.  Jalapeño aficionado from
the land Down Under.


Additional information
======================

I work on identity management solutions at Red Hat (primarily X.509,
Kerberos and LDAP).  Keycloak is also developed at Red Hat (by a
different team).  I presented this topic at PyCon Australia 2017.  I
have presented conference talks on a range of topics (primarily
identity management, security and functional programming) at many
conferences in Australia and abroad.

- PyCon AU 2014, 2015, 2017
- linux.conf.au 2015, 2016, 2017, 2018
- OSDC 2014, 2015
- DevConf.cz 2015
- CrikeyCon 2015, 2016, 2017, 2018
- AusCERT 2016
- YOW! Lambda Jam 2015, 2016, 2017
