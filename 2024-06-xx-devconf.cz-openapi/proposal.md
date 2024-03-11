# OpenAPI crash course

## Abstract

**OpenAPI** (formerly *Swagger*) is a specification for describing
HTTP APIs.  It supports automatic generation of server interfaces
and client bindings, in many programming languages.  This talk will
give an overview of OpenAPI and its use cases, and demonstrate the
tooling for some popular languages.  I will also discuss the "rough
edges" and compatibility issues we encountered using OpenAPI in a
multi-language project (Go, TypeScript, Python).

Programmers at any experience level who use or write HTTP APIs (in
any language) will get something out of this talk.


## Notes

The project which provided my OpenAPI experience is
[*Podengo*](https://github.com/podengo-project), an effort to bring
support for automatically joining VMs to identity management
(FreeIPA) domains in cloud environments.  Promoting this project is
not the purpose of this talk, but because it is the main **case
study** it will be described and discussed to some extent.
