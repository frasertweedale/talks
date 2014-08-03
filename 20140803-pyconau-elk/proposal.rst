..
  Copyright 2014  Fraser Tweedale

  This work is licensed under the Creative Commons Attribution 4.0
  International License. To view a copy of this license, visit
  http://creativecommons.org/licenses/by/4.0/.


Talk Title
==========

Descriptors: A Tool for Every Python Developer


Abstract
========

Descriptors are a powerful feature of Python that can be used to
augment or replace traditional attribute access with arbitrary
behaviours.  Such versatility makes the descriptor protocol a
valuable tool for any Python programmer, yet although most of us
have used descriptors, fewer have implemented one.

This presentation will explain the what, why and how of descriptors,
and mistakes to avoid.  We will begin with an overview of the
descriptor protocol and some descriptors with which audience members
are likely already familiar.

An in-depth, interactive look at the implementation of some simple
descriptors will follow.  Finally, the presentation will conclude
with a look at Elk_ - a declarative object system authored by the
presenter and inspired by Perl's Moose_ - and the descriptors that
power it.

.. _Elk: https://frasertweedale.github.io/elk/
.. _Moose: https://metacpan.org/module/Moose


Private Abstract
----------------

For the in-depth look at some simple descriptors, options to cover
include: default values, type constraints and method/attribute
delegation.  The implementation of ``property`` or some other common
standard library descriptor will also be instructive.


Target audience
===============

Developer - Intermediate


Project
=======

Elk

Project homepage
----------------

https://frasertweedale.github.io/elk/


Biography
=========

Fraser is a former Perl Monger who missed Moose so much he ported it
to Python.  Apart from an all-too-brief stint working on a financial
application in Python, he has mainly done Web Things in a variety of
other languages.  Although he is increasingly a believer in strong
type systems (of the Haskell kind), Fraser considers Python the most
interesting and enjoyable dynamically typed language.

Since April, Fraser works at Red Hat on the FreeIPA project, a
security information management suite.


Relevant Experience
===================

Should my proposal be accepted, PyCon Australia will be my first
talk at a conference, but I'm comfortable in front of a technical
crowd, having presented at several user groups including Brisbane
Perl Mongers, BrisPy and Brisbane Functional Programming Group.

In particular, my `recent presentation at BrisPy`_ was an
introduction to Elk, and included a live-coding session wherein a
functional subset of Elk was developed using TDD.  The resultant
code is available at https://github.com/frasertweedale/deer.  I felt
that this was an effective way to motivate and demonstrate the use
of metaclasses and descriptors, and therefore I will take a
similarly expository approach to demonstrating descriptors in my
PyCon Australia presentation.

.. _Recent presentation at BrisPy: https://github.com/frasertweedale/talks/tree/master/20130807-python-elk
