Secure development: does the language matter?
=============================================

Abstract
--------

Secure coding guides and checklists are useful for improving
software security (when followed).  They cover a lot of important
(mainly language-agnostic) topics.  But one topic they usually don't
cover is the question of what programming languages are best, or
what language features are important, for writing secure programs.
Industry-wide there seems to be little attention given to these
important questions.  This presentation will review the literature
on how languages affect program correctness, demonstrate some
practical examples of how language features can exclude (or enable)
certain kinds of vulnerabilities or weaknesses, and discuss how
vulnerability taxonomies such as the Common Weakness Enumeration
(CWE) could be useful in helping us choose languages and tools for
secure development.


Session outline
---------------

We will begin with a review of the (unfortunately quite limited)
literature on how different programming languages affect the
incidence of various classes of bugs/defects (as a proxy for
security) as well as maintainability.

Next I will present some specific examples of security bugs I
discovered and/or fixed.  We will examine the causes of these
issues, and discuss programming language features that could have
prevented or helped to prevent the weakness, or in some cases
enabled the weakness.  Some actionable secure development principles
will be realised from these case studies.

The talk will conclude with a discussion of software bug and
vulnerability taxonomies, in particular the Common Weakness
Enumeration (CWE).  We will ask whether CWE classifications attached
to CVEs, tickets or commit messages could help steer us toward
better programming tools and languages, or whether another taxonomy
of "how the issue could have been prevented" is warranted.


What will attendees learn?
--------------------------

The literature review should highlight to attendees the importance
of the question of how languages affect software correctness, and
the need for further empirical research in this area.

For programmers and engineering managers, there will be several
actionable takeaways to improve the correctness and security of the
code you write today, and help you choose the right languages and
tools for the code you'll write in the future.

Security analysts and programmers will learn the importance of
accurately describing not only the nature of a bug, but how it might
have been prevented.
