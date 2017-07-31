Sharing secrets securely and auditably
======================================

In today's cloudy, containerised, configuration-managed and
continuously-deployed world, securely sharing secrets with services
that need them remains a challenge.  There are several popular
solutions (etcd, HashiCorp Vault, et al.) but they all do things
differently, reducing interoperability and increasing complexity.

In this presentation I will expose some shortcomings of existing
approaches, then introduce Custodia, a "Secrets-as-a-Service" API
specification designed to simplify secret management, supporting
arbitrary backends and providing a consistent interface for
applications and audit regardless of the backend(s) used.

We will discuss the goals and design of Custodia and its reference
implementation, and look in detail at how it is being used to secure
secrets in the FreeIPA identity management system, and how it could
be integrated with other projects including Kubernetes and
OpenStack.


Private abstract
----------------

Custodia is developed by the Red Hat Engineering (Platform Security)
team.  I work closely with Custodia's developers, and the FreeIPA
identity management system (which I work on) uses Custodia for
securely transporting secrets in several contexts.
