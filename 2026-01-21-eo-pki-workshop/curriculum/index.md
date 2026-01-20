---
next-href: modules/01-keygen-and-csr.html
next-text: Key generation and CSR creation with OpenSSL
up-href: "#toc"
up-text: Go to index
---

# Practical PKI - a hands-on workshop

The Practical PKI workshop is designed to introduce you to PKI and
X.509 fundamentals, and real-world applications.  There are two main
application areas:

- TLS certificates for public websites and services (via ACME)
- Enterprise PKI for Linx / Unix environments (with FreeIPA)

::: note

Please [**give your feedback**](feedback.html) at the end of the session!

:::


## Prerequisites

The workshop is designed to make it as easy as possible to
participate.

You will need a computer with an **Internet access, an SSH client
and a web browser**.  You can use **any operating system**—most of
the exciting stuff happens on the machines in the workshop
environment.

Some prior experience with Linux / Unix and using a command shell
will be helpful, but it is not a strict requirement.

The smart card module has an **optional** remote graphical login
scenario, using *Remote Desktop Protocol (RDP)*.  Suitable clients
include:

- **Linux**: `gnome-connections` or `remmina` (install via your
  package manager)
- **Mac**: FreeRDP ([download](https://www.freerdp.com/) or
  `brew install freerdp`)
- **Windows**: mRemoteNG ([download](https://mremoteng.org/))


## Workshop modules {#toc}

The workshop activities are organised into modules.  Some are
prerequisites of others, but these dependencies should be fairly
clear.  You don't have to do all modules in the listed order; feel
free to explore the branches that interest you.

Foundational modules:

- [Key generation and CSR creation with OpenSSL](
    modules/01-keygen-and-csr.html)

Public PKI modules:

- [ACME certificates for Apache httpd with `mod_md`](
    modules/acme-httpd-mod_md.html)
- [Using Certificate Transparency logs](
    modules/certificate-transparency.html)

Enterprise PKI modules:

- [Service certificates with FreeIPA and Certmonger](
    modules/ipa-certmonger.html)
- [FreeIPA certificate profiles and user certificates](
    modules/ipa-profiles.html)
- [Smart cards and workstation login](
    modules/ipa-smart-cards.html)
- [Externally signing the FreeIPA CA](
    modules/ipa-external-ca.html)
- [The FreeIPA ACME server](
    modules/ipa-acme.html)


## Your unique workshop environment

You will have received a card bearing your participant number and
some access details.  Your workshop environment is hosted under the
domain `e$N.pki.frase.id.au` (where `$N` is your particpant number).

Throughout the curriculum, the variable `$DOMAIN` refers to your
environment's domain.

There are several machines in the environment:

- `server.e$N.pki.frase.id.au`: the FreeIPA server
- `client.e$N.pki.frase.id.au`: an enrolled client machine
- `web.e$N.pki.frase.id.au`: a web server

### Environment variables

On all three machines, login shells will automatically set the
`DOMAIN` and `REALM` environment variables.  For example:

```
export DOMAIN=e17.pki.frase.id.au
export REALM=E17.PKI.FRASE.ID.AU
```

You might also find it convenient to set these variables in your
shell session on your local machine.


### Accessing the environment

The machines in your environment can be accessed over SSH.  The
unique SSH private key for your environment is available for
**download at <https://pki-workshop.frase.id.au/envs/$N/>**.
Download your private key and provide it to your SSH client.  Then
you can log into any of the machines in your environment, using the
`fedora` user account.

If you use OpenSSH, the login command is:

```command
ssh -i path/to/key.pem fedora@client.e$N.pki.frase.id.au
```

Accept the host key prompt and log in.

::: note

Do not access or interfere with other participants' environments!

:::


### *hacker voice*: I'm in.

Now that you're here, here is some info about the machines.

- You are in the `fedora` user account
- You have full `sudo` access for performing actions as `root` (when
  needed)
- **Editors**: `vi` (Vim) and `nano` are available.  The default
  `EDITOR` is `nano`.
- The shell is Bash version 5.3.0
- `tmux` is installed


### FreeIPA credentials

For some workshop modules, you will access and perform
administrative actions in a FreeIPA domain.  The accounts and access
credentials are as follows:

- `admin` account: password = `Secret.123`
- `user1` account: password = `Secret.123`
