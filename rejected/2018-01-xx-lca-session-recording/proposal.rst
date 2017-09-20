User Session Recording for the Enterprise
=========================================

Abstract
--------

For Open Source software to conquer the enterprise, we need to play
along with government and industry regulations, and help
organisations meet their security and audit requirements.  Sometimes
this means tracking everything a user sees and does.  A flexible and
scalable Open Source user session recording solution is needed.

In this presentation we will discuss the limitations of existing
Open Source approaches, then present the *Scribery* project, an
end-to-end session recording solution with features including:

- terminal session playback and real-time monitoring (including what
  the user sees)

- centralised storage and correlation with auditd log events

- centralised control of what or whom to record, via SSSD and in the
  future FreeIPA

- Cockpit integration

The presentation will include a demo of a user session being
recorded, stored centrally, inspected and played back.

We will look at the architecture, discuss implementation challenges,
and conclude with an overview of the road ahead.

The intended audience is system administrators and security officers
responsible for security and compliance, and developers of security,
identity and policy management systems.

https://scribery.github.io/


Private abstract
----------------

The *Scribery* project is being developed by the Red Hat Engineering
(Platform Security) team.  I work in that team on FreeIPA, which
will be used for centralised control of session recording.
