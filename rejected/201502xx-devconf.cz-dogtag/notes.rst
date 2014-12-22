- Overview of PKI/CA certification chains.

- Look at the current way to have intermediate CAs with Dogtag.

- Use cases for API-driven sub-CA creation / administration
  - Multiple security domains for, e.g. user certs, Puppet.
  - Cloud infrastructure PKI needs.
    - Multi-tenancy of unrelated CAs / CA chains
  - **Barbican**
    - *nkinder*: consider Barbican in OpenStack.  Barbican is
      getting into certificate issuance now, but it's quite likely
      that separate tenants within a cloud do not want to trust each
      other.  Barbican backed by IPA/Dogtag could offer
      PKI-as-a-service, where each tenant could create their own
      root and then issue certificates for their
      services/applications within their instances.

- Design and implementation
  - Overview of API
  - Overview of sub-CA resources that are shared / not shared with
    parent.
  - CRL and OCSP considerations.
  - Key replication
    - Rejected design: replication over LDAP
    - Accepted design: key distribution via network service

- FreeIPA integration
  - Binding of user / server / host principals to security domains
  - Administration and authorization
  - Acquiring / publishing sub-CA signing certificates
