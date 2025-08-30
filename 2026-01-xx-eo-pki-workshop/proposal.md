# Practical PKI: A hands-on X.509 workshop

TLS and X.509 certificates are an integral part of Internet
security, yet their inner workings can feel like a black box.  In
this tutorial we will explore a variety of certificate use cases and
practice certificate management activities.  Attendees will gain an
understanding of Public Key Infrastructure (PKI) fundamentals, X.509
anatomy, and practical skills centred on FreeIPA.

Besides the pervasive TLS (SSL) WebPKI use case, X.509 certificates
are widely used in enterprise environments for Smart Card
authentication, Kerberos PKINIT, and 802.1X EAP system
authentication.  This makes X.509 and certificate management
essential knowledge for systems and network administrators and
DevOps engineers.

Using FreeIPA (Red Hat Identity Management) as our hands-on
platform, this tutorial will cover a variety of topics and scenarios
including:

- X.509 and PKI fundamentals (short presentation)
- Using OpenSSL to generating keys and create certificate signing
  requests (CSRs)
- ACME (Let's Encrypt) certificate management
- External signing and renewal of the FreeIPA CA
- Configuring FreeIPA certificate profiles and sub-CAs
- Smart Card authentication on Linux hosts

The session will conclude with a brief overview of current
directions in PKI and X.509 including ACME, Certificate
Transparency, the evolving revocation landscape and post-quantum
cryptography.

This is an intermediate-level workshop.  The intended audience is
systems and network administrators, operations engineers, security
practitioners, and anyone interested in web or network protocol
security.

Participants will be provided with access to preconfigured cloud
environments on which they will undertake the tutorial activities.
They will need a machine with Internet access and an SSH client, and
should be comfortable in a Unix command line environment.

Project URL: http://www.freeipa.org/
