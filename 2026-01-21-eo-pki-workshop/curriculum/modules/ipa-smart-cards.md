---
back-href: ipa-profiles.html
back-text: FreeIPA certificate profiles and user certificates
up-href: "../index.html#toc"
up-text: Up to index
next-href: ipa-external-ca.html
next-text: Externally signing the FreeIPA CA
---

# Smart cards and workstation login

*Smart cards* are cryptographic devices that can securely store keys
and certificates to enable a variety of authentication and
encryption applications.  Common use cases include:

- Physical access badges
- Workstation login (maybe you've seen this at a bank)
- Storing OpenPGP or SSH keys

Smart cards offer a common interface through the PKCS #11 standard.
TPMs and smartphone *secure elements* also provide this interface
and can behave like smart cards.  The principle is that keys cannot
be extracted from the hardware.

::: note

Software implementations are also possible, but offer none of the
physical security benefits.

:::

In this module you will use a TPM like a smart card and walk through
some real world scenarios:

- Generate a key on the device and sign a CSR

- Install the issued certifiate on the device

- Configure a FreeIPA domain and enrolled workstation to enable
  smart card login

- Configure *GNOME Remote Desktop* to enable remote graphical login

::: note

Most activities in this module are to be performed on
`workstation.$DOMAIN`. SSH into this machine now.

**You will also need an RDP client to perform the graphical
workstation login.**  You can still do most of the activities
without it, but you will miss out on some of the payoff.

:::


## Setting up the smart card


::: note

**TODO TODO TODO** update the machine image to include the following
packages (and until Fraser does that, install them NOW)

```command
sudo dnf install tpm2-pkcs11 tpm2-pkcs11-tools openssl-pkcs11
```

:::

::: note

On Fedora and RHEL, you will need the following packages:

- `tpm2-pkcs11`
- `tpm2-pkcs11-tools`
- `openssl-pkcs11`

These are already installed on the `workstation` VM.  Other
distributions may require different packages.

:::

The exact commands for initialising and configuring a smart card
will differ by vendor.  In this workshop we are using the TPM as a
smart card.  Some of the commands are TPM specific, and some are
generic.  Despite this, you should gain an understanding of the
general procedure required to use smart cards for X.509
applications.


### Initialise a smart card interface to the TPM

The first step is to initialise the TPM as a "smart card".

```command {.workstation}
sudo tpm2_ptool init
```
```output
action: Created
id: 1
```

That command creates a local database to track the tokens and keys
created in the TPM-backed smart card.  Next create a *token*:

```command {.workstation}
sudo tpm2_ptool addtoken --pid=1 \
  --sopin=AdminPIN123 \
  --userpin=UserPIN123 \
  --label="TPM-Token"
```

### Generate key pair

Now generate a private key (in this case, a NIST P-256 ECC key):

```command {.workstation}
sudo tpm2_ptool addkey \
  --label="TPM-Token" --userpin=UserPIN123 \
  --algorithm=ecc256 --key-label="ipa-key"
```
```output
action: add
private:
  CKA_ID: '31303237303332383133623634366232'
public:
  CKA_ID: '31303237303332383133623634366232'
```

`--label` identifies the token and `--userpin` unlocks it to enable
the operation.  The output shows the `CKA_ID`, which is an identifer
that will link the public key, private key, and (yet to be issued)
certificate.


### Create CSR

So far we have used commands specific to the TPM PKCS #11
implementation to set up our "smart card".  To create the CSR we
will use `openssl`.  But we must first find out the PKCS #11 URI for
our key, so that `openssl` can see it:

```command {.workstation}
sudo TPM2_PKCS11_BACKEND=esysdb p11tool \
  --list-all "pkcs11:token=TPM-Token"
```
```output
... some warnings (ignore them) ...
Object 0:
        URL: pkcs11:model=NitroTPMv1.0%00%00%00%00;manufacturer=AMZN;serial=0000000000000000;token=TPM-Token;id=%31%30%32%37%30%33%32%38%31%33%62%36%34%36%62%32;object=ipa-key;type=public
        Type: Public key (EC/ECDSA-SECP256R1)
        Label: ipa-key
        ID: 31:30:32:37:30:33:32:38:31:33:62:36:34:36:62:32
```

Copy the value of the `URL: ` field and save it in a shell variable.
**Be sure to wrap the value in quotes (`"`).**  For example:

```command {.workstation no-copy}
PKCS11_URI="pkcs11:model=NitroTPMv1.0%00%00%00%00;manufacturer=AMZN;serial=0000000000000000;token=TPM-Token;id=%31%30%32%37%30%33%32%38%31%33%62%36%34%36%62%32;object=ipa-key;type=public"
```

Now generate the CSR.  **You will be prompted for the user PIN**.

```command {.workstation}
sudo openssl req -new \
  -engine pkcs11 -keyform engine -key $PKCS11_URI \
  -config user_csr.cnf -out tpm-user.csr
```
```output
Engine "pkcs11" set.
... some warnings (ignore them) ...
Enter PKCS#11 token PIN for TPM-Token:
```


### Request and import certificate

Request the certificate from the CA.  Given the context, this might
also be referred to as *enrolment*.

```command {.workstation}
ipa cert-request user.csr \
    --profile-id userCert \
    --principal user1 \
    --certificate-out tpm-user.crt
```

Now use the `tpm2_ptool` command to import the certificate into the
smart card:

```command {.workstation}
sudo tpm2_ptool addcert \
  --label="TPM-Token" --key-label="ipa-key" tpm-user.crt
```
```output
action: add
cert:
  CKA_ID: '31303237303332383133623634366232'
```

The PIN is not required for this operation.  Note the `CKA_ID`
matches that of the keypair.


## Enable smart card login on the workstation

TODO


## Enable smart card login for FreeIPA users

TODO


## Enabling graphical login via RDP

To simulate a workstation smart card login experience, we will
enable [*Remote Desktop Protocol (RDP)*][wiki-rdp] login, using
***GNOME Remote Desktop***.

[wiki-rdp]: https://en.wikipedia.org/wiki/Remote_Desktop_Protocol

Wouldn't you know it, RDP uses TLS to secure the traffic between
client and server.  Using Certmonger, request a service certificate
for the RDP server to use:

```command {.client}
sudo ipa-getcert request \
    -f /etc/pki/tls/certs/rdp.crt \
    -k /etc/pki/tls/private/rdp.key \
    --key-owner gnome-remote-desktop \
    --cert-owner gnome-remote-desktop \
    -K host/$(hostname) \
    -D $(hostname)
```

Now tell GNOME Remote Desktop about the key and certificate:

```command {.workstation}
sudo grdctl --system rdp \
  set-tls-key  /etc/pki/tls/private/rdp.key
```

```command {.workstation}
sudo grdctl --system rdp \
  set-tls-cert /etc/pki/tls/certs/rdp.crt
```

::: note

You can ignore error messages that mention TPM credentials.

:::

We have to configure an RDP username and password.  These
credentials are unrelated to FreeIPA or system accounts:

```command {.workstation}
sudo grdctl --system rdp
  set-credentials rdp hunter2
```

Allow RDP traffic through the firewall:

```command {.workstation}
sudo firewall-cmd --permanent --add-service=rdp
```

```command {.workstation}
sudo firewall-cmd --reload
```

Finally, enable the service:

```command {.workstation}
sudo grdctl --system rdp enable
```


## Connecting to RDP

Use your RDP client to connect to
`workstation.env$N.pki.frase.id.au`.  You may need to prefix the
domain name with `rdp://`.  The TCP port is `3389`.

You may need to accept the server's certificate—which you issued and
configured!
