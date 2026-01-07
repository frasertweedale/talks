# Module ???: Service certificates with FreeIPA and Certmonger

The **FreeIPA** identity management system (also available in RHEL
as **Red Hat Identity Management**) provides an enterprise PKI for
issuing and managing host, service and user certificates.

In this module you will use the **Certmonger** tool to issue and
renew a certificate for the host principal.

::: note

All steps in this module are to be performed on `client.$DOMAIN`,
where you generated keys and CSRs.  If you are not already there,
SSH into this machine now.

:::


## Environment overview

`client.$DOMAIN` already enrolled as a client in the FreeIPA domain.
That means it has a corresponding *host principal* object in the
domain (if you're familiar with Microsoft Active Directory, *machine
account* is the equivalent).  Before proceeding, let's inspect this 
object.

FreeIPA uses the **Kerberos** protocol for authentication.
Authenticate as user `user1` (initial password = `Secret.123).  You
will be prompted to set a new password when authenticating for the
first time (you can use the same password).

```
[fedora@client ~]$ kinit user1
Password for user1@E1.PKI.FRASE.ID.AU:
Password expired.  You must change it now.
Enter new password:
Enter it again:

[fedora@client ~]$ ipa host-show $(hostname)
  Host name: client.e1.pki.frase.id.au
  Platform: x86_64
  Operating system: 6.17.1-300.fc43.x86_64
  Principal name: host/client.e1.pki.frase.id.au@E1.PKI.FRASE.ID.AU
  Principal alias: host/client.e1.pki.frase.id.au@E1.PKI.FRASE.ID.AU
  SSH public key fingerprint: ...
  Password: False
  Keytab: True
  Managed by: client.e1.pki.frase.id.au
```

(Some details elided.  Also, your domain and realm names will differ
from the above).

For the user certificate, you will submit the `user.csr` you created
in the **Key generation and CSR creation** module.  The `user1` user
account already exists in the FreeIPA domain>

You will use the `admin` account to perform administrative actions
in the FreeIPA domain.  The password of the `admin` account is
`Secret.123`.


## Scenario 1: Host certificate

In this scenario: you'll use the ***Certmonger*** program to request
and manage (**renew**) a certificate for an enrolled host from the
FreeIPA CA.  A real world use case for this might be to enable
802.1X network authentication.


### Preparation

Enable and start Certmonger:

```
[fedora@client ~]$ sudo systemctl enable --now certmonger
  Created symlink /etc/systemd/system/multi-user.target.wants/certmonger.service → /usr/lib/systemd/system/certmonger.service.
```


### Request certificate

Use the `ipa-getcert` command, which is part of Certmonger, to
request a certificate.  Certmonger will automatically perform the
following steps:

1. Generate a private key
2. Sign a CSR (ephemeral)
3. Submit the CSR to the FreeIPA CA for signing
4. Save the issued certificate
5. Monitor the certificate and renew it before expiry

```
[fedora@client ~]$ sudo ipa-getcert request \
    -f /etc/pki/tls/certs/host.crt \
    -k /etc/pki/tls/private/host.key \
    -K host/$(hostname) \
    -D $(hostname)
  New signing request "{TRACKING_ID}" added.
```

::: note

Record the signing request identifier that appears in the command
output.  You will need it later.  For example:

```
[fedora@client ~]$ TRACKING_ID=20260107053408
```

:::

Let's break down some of those command arguments.

`-k <path>`
: Path to private key (Certmonger will generate it)
`-f <path>`
: Path to certificate (where it will be saved after being issued)
`-K <principal>`
: Kerberos host or service principal; because different kinds of
  services may be accessed at one hostname, this argument tells
  Certmonger which service principal is the certificate subject
`-D <dnsname>`
: Requests the given domain name to appear in the *Subject
  Alternative Name (SAN)* extension; today the *Common Name (CN)*
  field is no longer used by browsers so the SAN value is essential

Another important option is `-N <subject-name>`.  It defaults to the
system hostname, which is appropriate for our use case.

Check the status of the Certmonger request using tracking ID
from the `ipa-getcert request` output:

```
[fedora@client ~]$ sudo getcert list -i $TRACKING_ID
Number of certificates and requests being tracked: 1.
Request ID '{TRACKING_ID}':
  status: MONITORING
  stuck: no
  key pair storage: type=FILE,location='/etc/pki/tls/private/host.key'
  certificate: type=FILE,location='/etc/pki/tls/certs/host.crt'
  CA: IPA
  issuer: CN=Certificate Authority,O=E1.PKI.FRASE.ID.AU
  subject: CN=client.ipademo.local,O=E1.PKI.FRASE.ID.AU
  issued: 2026-01-07 05:34:08 UTC
  expires: 2028-01-08 05:34:08 UTC
  dns: client.e1.pki.frase.id.au
  principal name: host/client.ipademo.local@IPADEMO.LOCAL
  key usage: digitalSignature,nonRepudiation,keyEncipherment,dataEncipherment
  eku: id-kp-serverAuth,id-kp-clientAuth
  pre-save command:
  post-save command:
  track: yes
  auto-renew: yes
```

Review the output to confirm that:

- The certificate was issued and that Certmonger is now
  `MONITORING`.
- Certmonger will `auto-renew` the certificate when it is
  close to expiring.

FreeIPA adds the issued certificate to the service entry (technical
detail: it is in the LDAP `userCertificate` attribute).  It now
appears in the `ipa host-show` output:

```
[fedora@client ~]$ ipa host-show $(hostname)
  Host name: client.e1.pki.frase.id.au
  Platform: x86_64
  Operating system: 6.17.1-300.fc43.x86_64
  Certificate: MIIFgDCCA+igAwIBAgIQOJ... (it's big!)
  Subject: CN=client.e1.pki.frase.id.au,O=E1.PKI.FRASE.ID.AU
  Serial Number: 75212939205584776743200657781018594668
  Serial Number (hex): 0x38957C34E5953F0FB537BB73C7440D6C
  Issuer: CN=Certificate Authority,O=E1.PKI.FRASE.ID.AU
  Not Before: Wed Jan 07 05:34:08 2026 UTC
  Not After: Sat Jan 08 05:34:08 2028 UTC
  Fingerprint (SHA1): 27:92:a6:69:64:c5:ec:7a:88:11:46:ea:ad:cd:64:74:50:4b:51:1e
  Fingerprint (SHA256): 75:a1:d5:b1:ab:79:65:79:88:4a:a5:87:c5:d2:54:c6:4f:02:d1:3c:ad:9f:ab:54:26:70:57:e8:53:df:10:c9
  Principal name: host/client.e1.pki.frase.id.au@E1.PKI.FRASE.ID.AU
  Principal alias: host/client.e1.pki.frase.id.au@E1.PKI.FRASE.ID.AU
  ...
```

### What is actually happening?

Under the hood, Certmonger uses the *host keytab* (acquired upon
joining the FreeIPA domain) to issue an `ipa cert-request` command
to the FreeIPA management API.  FreeIPA authorises the request
(hosts can **self-service** certificate requests) and if everything
looks good, it passes the CSR along to the **Dogtag CA** for
signing.  It stores the signed certificate in the `userCertificate`
attribute on the subject principal LDAP entry, and also returns the
certificate to the client that performed the request.

The *operator* who executes the `cert-request` is not necessarily
the subject.  Host principals can also request certificates for
*service principals* managed by that host.


### Forcing renewal

Certmonger will automatically renew the certificate when it is close
to expiry.  But you can use the `getcert resubmit` command if you
want to renew it immediately:

```
[fedora@client ~]$ sudo getcert resubmit -i $TRACKING_ID
Resubmitting "20260107053408" to "IPA".
```

After a moment, the renewal will be complete.  `getcert list` shows
the updated validity period:

```
[fedora@client ~]$ sudo getcert list -i $TRACKING_ID
Number of certificates and requests being tracked: 1.
Request ID '20260107053408':
        status: MONITORING
        subject: CN=client.e1.pki.frase.id.au,O=E1.PKI.FRASE.ID.AU
        issued: 2026-01-07 05:44:20 UTC
        expires: 2028-01-08 05:44:20 UTC
        dns: client.e1.pki.frase.id.au
        ...
```


## Scenario 2: User certificates

### Configure CA ACLs

`kinit` as `admin`.
