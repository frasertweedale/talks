# Practical PKI - a hands-on workshop

The Practical PKI workshop is designed to introduce you to PKI and
X.509 fundamentals, and real-world applications.  There are two main
application areas:

- TLS certificates for public websites and services (via ACME)
- Enterprise PKI for Linx / Unix environments (with FreeIPA)


## Prerequisites

The workshop is designed to make it as easy as possible to
participate.

You will need a computer with an **Internet access, an SSH client
and a web browser**.  You can use **any operating system**—all the
exciting stuff happens on the machines in the workshop environment.

Some prior experience with Linux / Unix and using a command shell
will be helpful, but it is not a strict requirement.


## Workshop modules

The workshop activities are organised into modules.  Some are
prerequisites of others, but these dependencies should be fairly
clear.  You don't have to do all modules in the listed order; feel
free to explore the branches that interest you.

Foundational modules:

- [Key generation and CSR creation with OpenSSL](
    modules/01-keygen-and-csr.html)

Public PKI modules:

- [ACME certificate for Apache httpd with `mod_md`](
    modules/acme-httpd-mod_md.html)

Enterprise PKI modules:

- [Service certificates with FreeIPA and Certmonger](
    modules/ipa-certmonger.html)
- [FreeIPA certificate profiles and user certificates](
    modules/ipa-profiles.html)
- [External signing of the FreeIPA CA](
    modules/ipa-external-ca.html)
- [Smart Card login](
    modules/ipa-smart-cards.html) (TODO)


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

::: note

Do not access or interfere with other participants' environments.

:::


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
