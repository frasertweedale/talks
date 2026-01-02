# Module 1: Key generation and CSR creation with OpenSSL

## Introduction

Before a Certificate Authority (CA) can issue you an X.509
certificate, you must first create a **Public/Private Key Pair** and
a **Certificate Signing Request (CSR)**. The private key is the
secret component that proves your identity, and the CSR contains
your public key and identity information (like a website's domain
name or a user's email address) that the CA will embed in the final
certificate.

In this module you'll use the `openssl` command line tool to
generate keys and create CSRs.

## 1. Server CSR with RSA key

Let's prepare a CSR suitable for an HTTP server.  We will use a
strong RSA key and include the DNS hostname in both the Common Name
(CN) field and the Subject Alternative Name (SAN) extension.

### A. Generate the RSA Private Key

We will use the **RSA 3072-bit** key size, which is the minimum RSA
key size currently recommended by NIST for sercure services.  (TODO
reference).

```
% openssl genpkey \
    -algorithm RSA \
    -pkeyopt rsa_keygen_bits:3072 \
    -out service.key
```

TODO key encryption!

### B. Create config file for service CSR

Create a configuration file to tell `openssl` what content to
include in the CSR.  For service certificates, the identity
requirements are often minimal, focusing only on the DNS name. We'll
use the domain name `server1.ipa.test`.

Create a file named `service_csr.cnf` with the following content:

```
[ req ]
default_bits        = 3072
prompt              = no
default_md          = sha256
req_extensions      = req_ext
distinguished_name  = dn

[ dn ]
# NOTE: In real-world use cases, you may need to include other
# attributes (Country, Organization, etc.)
commonName = server1.ipa.test

[ req_ext ]
subjectAltName = @alt_names

[ alt_names ]
# The DNS name MUST match the Common Name for best practice.  
DNS.1 = server1.ipa.test
```

### C. Generate the service CSR

Execute the `openssl req` command below to build a CSR according to
the config file and sign it with the private key:

```
% openssl req -new \
    -key service.key \
    -config service_csr.cnf \
    -out service.csr
```

### D. Verify the service CSR

Always verify your CSRs before submission to ensure the required
extensions and names are correctly included.

Check the SAN and key parameters for the service request.

```
% openssl req -in service.csr -text -noout
```

Look for the following in the output:

```
        Subject Public Key Info:
            Public Key Algorithm: rsaEncryption
                Public-Key: (3072 bit)

```

…and…

```
            Requested Extensions:
                X509v3 Subject Alternative Name:
                    DNS:server1.ipa.test
```


## 2. CSR for user certificate with ECC key

X.509 certificates can be used for a variety of user authentication
scenarios (e.g. Kerberos, VPN access, email signing).  Let's
generate an elliptic curve key and use it to sign a CSR that
includes the user's username and email address.

### A. Generate the ECC Private Key

We will use the **secp384r1** curve, which is recommended for high
security with efficient performance.  

```
% openssl genpkey \
    -algorithm EC \
    -pkeyopt ec_paramgen_curve:secp384r1 \
    -out user.key
```


### B. Create config file for user CSR

For user authentication certificates, the primary identifiers are
usually the username (in CN) and the email address (in the SAN).

Create a file named `user_csr.cnf` with the following content:

```
[ req ]
default_bits        = 384
prompt              = no
default_md          = sha384
req_extensions      = req_ext
distinguished_name  = dn

[ dn ]
commonName = user1

[ req_ext ]
subjectAltName = @alt_names

[ alt_names ]
email = user1@ipa.test
```

### C. Generate the User CSR

```
% openssl req -new \
    -key user.key \
    -config user_csr.cnf \
    -out user.csr \
```

### D. Verify user CSR

```
% openssl req -in user.csr -text -noout
```

Verify the elliptic curve parameters in the *Subject Public Key
Info* section, and the inclusion of the user's email address in the
*X509v3 Subject Alternative Name* extension.


## Key Takeaways

1. You can choose the **key type and size**.  Some CAs or
   organisational policies may restrict which key types or
   parameters are allowed.  Current NIST standards require 3072-bit
   RSA or a 256-bit elliptic curve keys as a minimum from 2031.

2. **CSR Customization:** The configuration file allows you to
   explicitly define the **Subject Distinguished Name (DN)** and
   **Subject Alternative Name (SAN)** attributes.

3. **Use case-specific SANs:** Use DNS.\* for services and email
   (rfc822Name) for user authentication certificates.  There are
   many other types of SAN values, including IP addresses (for
   servers accessed directly by IP).

**What's next?** You now have two distinct CSRs (`service.csr` and
`user.csr`).  We are ready to move on to the next module: submitting
these CSRs to our FreeIPA CA and receiving signed X.509
certificates!
