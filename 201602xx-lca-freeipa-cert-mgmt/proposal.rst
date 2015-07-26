..
  Copyright 2015  Red Hat, Inc.

  This work is licensed under the Creative Commons Attribution 4.0
  International License. To view a copy of this license, visit
  http://creativecommons.org/licenses/by/4.0/.


Talk Title
==========

Certificate Management in FreeIPA: Past, Present and Future


Abstract
========

FreeIPA is a centralised identity management solution providing
user, host and service management, authentication and authorisation
for Unix environments.  It is built on top of well-known Open Source
technologies and standards including 389 Directory Server, MIT
Kerberos and Dogtag Certificate System which provides an X.509 PKI
for the issuance of server and user certificates.

In this talk we will explore the past, present and future of
certificate management in FreeIPA.  I will:

- Describe how FreeIPA and Dogtag are integrated, and the process of
  certificate issuance.

- Look at the recent past in which FreeIPA supported a single
  certificate profile and could only issue a single certificate to
  hosts and services.

- Discuss the implementation of recently added features and the use
  cases that drove them: custom certificate profiles, sub-CAs, user
  certificates and support for adding certificates issued by
  external CAs.  Some of these features will be demonstrated in a
  live demo.

- Identify shortcomings in the current architecture of FreeIPA -
  Dogtag integration and present a vision for the future of
  certificate management in FreeIPA.

FreeIPA administrators, people evaluating FreeIPA, security
researchers and people interested in security who have a basic
understanding of X.509 PKI will get the most out of this talk.


Private Abstract
================

On the back of a very popular general presentation about FreeIPA at
linux.conf.au 2015, this year I hope to delve deeper into the
particular parts that I work on and teach people about a variety of
certificate use cases and the implementations - or envisaged
implementations - to solve them.


Target audience
===============

Developer


Relevant experience
===================

I have experience presenting at PyCon Australia, OSDC,
linux.conf.au, DevConf.cz and other conferences, as well as various
Brisbane user groups, on a variety of topics.

I take pride in delivering down-to-earth presentations and authentic
live demos.
