Defect to doctrine: security bug case studies
=============================================

I am not a security research or bug bounty hunter.  Just a humble
programmer who has found and fixed some security bugs over the
years.  I admire exploit authors who take advantage of programmers'
mistakes to penetrate, pivot and profit.  But this is not that talk.

As an industry, we can (and must) improve the security of the the
systems we write by learning from our mistakes.  Every bug tells a
story.  Every story has a moral, if you care to look for it.

In this talk I will describe four different vulnerabilities in
programs I worked on, including FreeIPA, Dogtag PKI and Firefox.  I
will explain what the bug was, it's impact, how it was discovered
and how I resolved it.  From each case study I will develop one or
two important principles for secure programming.

This presentation will be most useful for programmers, engineering
managers, and security folk who want an engineer's perspective on
how issues arise and how to avoid them.

Why choose this presentation?
-----------------------------

On the one hand, there's nothing new or extraordinary here.  They're
all bugs we've seen before, and neither are the takeaways novel.  On
the other hand, I can't recall a general secure programming at a
previous CrikeyCon (maybe I missed it).  And I think a "from the
trenches" storytelling approach to describing these vulns and their
implications will make an engaging presentation.  Conservative use
of (pseudo)code or diagrams, and maybe one or two live demos, will
illustrate the stories.  And I promise that one of the bugs, and the
story of how it was discovered, is pretty hilarious :)

For the program committee's awareness, the four vulns boil down to:
(1) insufficient authorisation combined with lack of privilege
separation; (2) boolean blindness leading to incorrect authorisation
decisions; (3) buffer overflow; (4) missing return value check.


Bio
---

Fraser works at Red Hat on the FreeIPA identity management system
and Dogtag Certificate System. He's interested in security,
cryptography and functional programming. Jalapeño aficionado.
