4 vulns: lessons for secure development
=======================================

I am not a 1337 h4x0r bug bounty hunter.  Just a humble programmer
who found and fixed some vulns (hopefully more than I have written,
but there's no way to be sure!) I have profound admiration for the
exploit author who can (ab)use tiny chinks in the armour to
penetrate, pivot and profit.  But this is not that talk.

What have I, as a programmer, learned from vulnerabilities I
discovered or helped fix?  Every bug tells a story.  Every story has
a beginning and (hopefully) an end.  Every tale has a moral, if you
care to search for it.

In this talk I will describe four different vulnerabilities in
programs I worked on, including FreeIPA, Dogtag PKI and Firefox.  I
will explain what the technical problem was, it's impact, how it was
discovered and how I resolved it.  From each case study I will
derive one or two universal principles for secure programming.

Programmers and engineering managers will get the most out of this
talk.  Other security folks who wonder "how come are there so many
vulns?" are likely to enjoy it too.

Why choose this presentation?
-----------------------------

On the one hand, there's nothing new or extraordinary here.  They're
all bugs we've seen before, and neither are the takeaways novel.  On
the other hand, I can't recall a general secure programming at a
previous CrikeyCon (maybe I missed it).  And I think a "from the
trenches" storytelling approach to describing these vulns and their
implications will make for an engaging presentation.  Conservative
use of (pseudo)code or diagrams, and maybe one or two live demos,
will illustrate the stories.  I am not a humourous fellow but I'll
bet you, the program committee, a round of beers that some of these
stories get a laugh.

For the program committee's awareness, the four vulns boil down to:
(1) insufficient authorisation combined with lack of privilege
separation; (2) boolean blindness leading to incorrect authorisation
decisions; (3) buffer overflow; (4) missing return value check.


Bio
---

Fraser works at Red Hat on the FreeIPA identity management system
and Dogtag Certificate System. He's interested in security,
cryptography and functional programming. Jalapeño aficionado.
