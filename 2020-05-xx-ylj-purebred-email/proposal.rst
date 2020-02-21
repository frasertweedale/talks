Five principles of data library design, illustrated
===================================================

Author:
  Fraser Tweedale

Session format:
  talk

Audience:
  beginner

Target audience:
  developers

Category:
  Technique


Abstract
--------

What should an industry-grade data format library look like?

In this presentation I will discuss the design of the
*purebred-email* library as a case study in data format API design.
My aim is threefold: to present some solutions to specific
email-related problems; to provide a data point in the wider space
of data format implementations (for when the audience member
inevitably has to implement one of their own); and to stimulate
thought and discussion on library ergonomics and usability.

I will begin with a manifesto: five principles of data library API
design.

From there, we will examine how those principles are applied in
three aspects of the *purebred-email* library:

- Parsing; how do you provide a nice interface to parsing data with
  many possible payload types?  What if you need to parse part of
  the structure to decide how to interpret the remainder?

- Reading the data; now that you have parsed the multi-level,
  possibly recursive structured data, how do you get at it?  How can
  you alter the data?  Adding or removing fields in keyed map-like
  structures?

- Payload encoding; how can a library handle data with arbitrary,
  possibly unknown encodings or character sets?

I will conclude with some thoughts on library ergonomics and
usability.  How do you trade off between power (versatility) and
usability (discoverability and affordance)—or is that a false
dichotomy?  I will discuss the benefits and drawbacks of generality
and the critical role of documentation.

Learning outcome
----------------

The areas of focus are applicable to many different data formats.
Attendees will learn viable approaches to solving these problems,
regardless of their data format or programming language.  Attendees
will learn about tradeoffs, consequences and kindness in library API
design, through elaboration of the five principles.


Outline / structure
-------------------

- Five principles of data library API design
- Parsing email data
- Processing email data
- Handling arbitrary encodings
- Usability and documentation
