..
  Copyright 2014  Red Hat, Inc.

  This work is licensed under the Creative Commons Attribution 4.0
  International License. To view a copy of this license, visit
  http://creativecommons.org/licenses/by/4.0/.


Topic
=====

**Lightweight sub-CAs in Dogtag Certificate System**


Abstract
========

Dogtag Certificate System currently supports one CA per instance,
but this is changing.  This talk will cover the motivation, design
and implementation of support for sub-CAs in Dogtag, with a view to
multi-tenancy of unrelated CA heirarchies in the future.

We will examine use cases from FreeIPA and the OpenStack Barbican
project that highlight some limitations of Dogtag's current
deployment model and justify a lightweight mechanism for creating
sub-CAs within an existing Dogtag instance.

Details of internal changes needed to support multiple CAs will be
given, including the CRL and OCSP systems, structure of the
certificate database and distribution of signing private keys among
replicas.

The API for creating and administering sub-CAs will be detailed,
along with other changes to external interfaces.  The upcoming
FreeIPA integration work will be discussed in brief, and the talk
will conclude with look at what remains to be done, and possible
future directions and use cases for multiple CAs in Dogtag.

Project URL: http://pki.fedoraproject.org/


Proposed category
=================

Security


Presenter bio
=============

Fraser is a developer at Red Hat in Brisbane, Australia where he
works on the FreeIPA identity management suite and Dogtag
Certificate System.  He is passionate about security and privacy.
In his free time Fraser enjoys jogging, watching football and
exploring the world of functional programming.
