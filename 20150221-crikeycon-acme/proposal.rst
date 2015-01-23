Let's Encrypt and the ACME protocol
===================================

Securing public web servers with TLS is a complex process that
involves:

0. Pay $$$ to a *certificate authority*.
1. Prove ownership of a domain; typically an ad-hoc manual
   procedure.
2. Request a certificate; many different request formats and
   enrolment protocols exist.
3. Configure the server to use the certificate; not as
   straightforward as it ought to be.

Is it any wonder so many sites remain unsecured?

`Let's Encrypt`_ is an initiative to deliver TLS everywhere by
establishing a free certificate authority and providing new tools to
automate domain validation and certificate provisioning.  A mid-2015
launch is planned, the project being driven by the EFF, Mozilla,
Akamai, Cisco and IdenTrust.

At the heart of Let's Encrypt is *Automated Certificate Management
Environment* (ACME), a JSON-over-HTTPS protocol that defines
validation challenges and responses, certificate issuance and
revocation messages (spec_).

This talk will explain ACME, examine its strengths, weaknesses and
limitations, and discuss client and server implementation
considerations.  A live demo of the complete ACME workflow from
domain validation to certificate installation and server
reconfiguration will give the audience a taste of what it will be
like to use ACME.  The talk will conclude with a look at possible
future use cases and suggestions on how to contribute to the success
of Let's Encrypt.

.. _Let's Encrypt: https://letsencrypt.org/
.. _spec: https://github.com/letsencrypt/acme-spec


Speaker Bio
-----------

Fraser Tweedale

Fraser works on Dogtag PKI and FreeIPA IdM in the employ of Red Hat.
He is passionate about functional programming, security and privacy
and has strong feelings about the (un)suitablility of JSON for
crypto formats.


Supporting information
----------------------

Technical overview of ACME:
https://letsencrypt.org/howitworks/technology/

The software used for the demo will be the ``node-acme`` server
(https://github.com/letsencrypt/node-acme) and the
``lets-encrypt-preview`` client
(https://github.com/letsencrypt/lets-encrypt-preview) which will
perform DVSNI validation and reconfigure Apache to enable SSL.
