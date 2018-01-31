No way JOSE! Lessons for standards authors and library developers
=================================================================

Description
-----------

(max 400 chars)

Protocol and data format specifications may be ambiguous, insecure
or problematic in other ways. Programmers and users bear the brunt
of these shortcomings. Using JOSE as a case study, I'll discuss
mistakes for standards authors to avoid, and demonstrate programming
techniques for mitigating some kinds of problems.

Topic
-----

Software methodologies

Secondary topics
----------------

Security, Software development

Session type
------------

40-minute session

Abstract
--------

(3..6 para)

Protocols and data formats are a programmer's bread and butter.
Sometimes their specifications are ambiguous, insecure or
problematic in other ways.  Programmers and users bear the brunt of
these shortcomings.

JOSE (JSON Object Signing and Encryption) is a set of IETF standards
for JSON-based cryptographic objects. You might know it as JWT or
JWS. JOSE appeared a few years ago and has been causing headaches
for the presenter ever since.

Using JOSE as a case study, this presentation looks at mistakes to
avoid when specifying a data format or cryptography standard. We'll
also explore programming techniques for mitigating some kinds of
problems in specifications.  In particular, we will cover:

* the flawed rationale for the JOSE working group
* why JSON is a poor wire format for cryptographic objects
* cryptography issues in the specifications
* ambiguities and interoperability problems in the specifications
* common vulnerabilities in JOSE libraries
* how library authors can encourage or enforce safe use
* advice for standards authors or working groups

Programming principles and techniques will be demonstrated using
Haskell and its _jose_ library, which is maintained by the author.


Who is the presentation for?
----------------------------

software developers


Audience level
--------------

Intermediate


What's the takeaway for the audience?
-------------------------------------

Learn about some mistakes to avoid when specifying data formats or
protocols, and software implementation techniques for dealing with
some of these issues.


Prerequisite knowledge
----------------------

No cryptography knowledge is required. Attendees with a passing
familiarity with static type systems and generics (i.e. you know
what they are) will get the most out of this talk.


Conceptual or how-to?
---------------------

Conceptual


Video URL
---------

https://www.youtube.com/watch?v=5S8ms93mVC4
