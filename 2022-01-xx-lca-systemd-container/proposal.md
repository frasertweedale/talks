# Send in the chown()s - systemd containers in user namespaces

## Abstract

"systemd in a container - what! why!?"  We've got our Reasons, and
I'll even explain them.  But more interesting than the "why" is the
"how", and that's what this talk is about.  Come and learn about the
upcoming and in-development Kernel and Kubernetes security features
that will enable better container isolation and secure deployment of
systemd-based workloads.

This is a talk about what happened when a handful of complete
container newbies tried to port their massive, complex, "legacy"
application to Kubernetes.  As a single, "monolith" container.
Based on systemd.

The container runtime shunned our application.  "Cloud engineers"
howled in dismay at our architecture decisions.  Ultimately, like
the hackers we are, we ignored their admonitions and doubled down.
If the container runtime won't run our application, well, we'll just
modify the container runtime!

And so we did.  Our journey took us into the darkest corners of
container runtimes, Kubernetes and systemd.  And we have emerged to
tell you the tale.  There will be demos.

Attendees should expect to learn more about the security
technologies that underpin Linux containers, including namespaces
and cgroups, as well as the behaviour of systemd in containers.

## Private abstact

I will be performing live (or recorded) demos of the features.  This
will include poking around on the container host to verify sandbox
properties.  As a consequence the audience will have a more complete
understanding of the security features that underpin Linux container
technologies.

The application being ported is FreeIPA - an identity management
solution.  The target environment is OpenShift, Red Hat's
distribution of Kubernetes. 

## Bio

Fraser works on security and identity solutions at Red Hat.  He is a
fan of functional programming and prefers pair programming with
powerful compilers.  Outside of computers he enjoys art, music and
little plastic bricks made in Denmark.
