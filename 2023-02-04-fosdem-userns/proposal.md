# Send in the chown()s - systemd containers in user namespaces

## Short abstract

Linux container escapes continue to affect Kubernetes and derived
products. User namespaces are one technology that can mitigate the
risk. In this presentation I will explain the past, present and
future of user namespace support in Kubernetes, and discuss how to
run systemd-based containers in user namespaces. And why you would
even want to try. There will be demos!  Attendees will learn about
what containers are, the technologies that underpin Linux
containers, and how Kubernetes actually runs containers.

## Abstract

"systemd in a container? What! Why!?" We had our Reasons, and I'll
even explain them.  But more interesting than the "why" is the
"how", and that's what this talk is about.  Come and learn about the
upcoming and in-development Kernel and Kubernetes security features
that will enable better container isolation and secure deployment of
systemd-based workloads.

This is a talk about what happened when a handful of complete
container newbies tried to port their massive, complex, legacy
application to Kubernetes.  As a monolithic container.  Based on
systemd.

The runtime shunned our container and refused to execute it.  Cloud
engineers recoiled in horror at our architecture.  With astounding
hubris we ignored their admonitions and doubled down.  If the
container runtime won't run our application, well, we'll just modify
the container runtime!

And so we did.  Our journey took us into the darkest corners of
container runtimes, Kubernetes and systemd.  And we have emerged to
tell you the tale.  There will be demos.

Attendees will learn about the security technologies that underpin
Linux containers, including namespaces and cgroups, as well as the
behaviour of systemd in containers.  I will also discuss the recent
and planned changes in Kubernetes to provide official support for
running containers in user namespaces.

## Private abstact

I will be performing live (or recorded) demos of the features.  This
will include poking around on the container host to verify sandbox
properties.  As a result, the audience will have a more complete
understanding of the security features that underpin Linux container
technologies - user namespaces and cgroups in particular.

The application being ported was FreeIPA - an identity management
solution.  The target environment is OpenShift, Red Hat's container
orchestration platform which is built on Kubernetes.
