# Passwordless Linux - Passkey and External IdP support in FreeIPA

The authentication landscape is changing, and a lot of work has been
done in Linux authentication technologies to keep up.  Learn how
FreeIPA and SSSD have grown support for FIDO2 passkeys and for
authenticating users from external OAuth 2.0 identity providers.
There will be demos!

Authentication is a critical aspect of host, network and
organisational security.  Identity management systems like FreeIPA
centralise your identities and access policies, to help you meet
your security and compliance requirements.

Historically and up to the present day, passwords were widely used
for initial user authentication.  But in the current era a healthy
security posture often demands 2FA, hardware cryptographic tokens,
consumption of identity assertions from third party providers, or
some combination of these.  Major trends in the web authentication
landscape include *Passkeys*, and delegation of authentication to
third-parties (sometimes called "Web SSO").

*Passkeys* is a convenient, passwordless, phishing-resistant
authentication technology based on FIDO Alliance standards.  It uses
public key cryptography and the credential can be implemented in
software or hardware.

*OAuth 2.0* is an access delegation protocol widely used on the web.
You have probably seen services that offer "Log in with {popular
site}".  When you use these options, OAuth 2.0 (or the OpenID
Connect protocol which builds upon it) are what happens behind the
scenes.  OAuth 2.0 also support non-web applications via the *Device
authorisation grant* flow.

Recent releases of FreeIPA and its client-side companion SSSD added
support for both of these authentication technologies.  In this
presentation I will review how these mechanisms work, and describe
what had to be added or changed in FreeIPA and SSSD to support them.
Attendees will learn, via practical demonstrations, how to configure
their own systems and networks to use these modern, secure
authentication mechanisms.
