..
  Copyright 2014  Fraser Tweedale.

  This work is licensed under the Creative Commons Attribution 4.0
  International License. To view a copy of this license, visit
  http://creativecommons.org/licenses/by/4.0/.


****************************************
Descriptors - attribute access redefined
****************************************

Fraser Tweedale
***************

About me
========

- Developer at Red Hat.

- FreeIPA identity management and Dogtag PKI.

- Mostly Python and Java at work.

- Mostly Haskell at home.


About this talk
===============

- What problems do descriptors solve?

- How are descriptors implemented, and used?

- Show me something cool that uses descriptors!

- Assumed knowledge: *clases*, *decorators*.  Familiarity with
  ``__getattr__`` and ``__setattr__`` will help.


Motivation
==========

- Attribute access is an *abstraction*.

- Python's default behaviour is to read and write values in an
  *instance dictionary*.

- The default implementation is well understood and sometimes
  appropriate.

- It is often useful to *augment* or *replace* the default
  behaviour.
  - ORMs or other data abstractions
  - Type checking and data validation
  - Callbacks (for extensibility, audit, ...)
  - things I haven't thought of...


Motivation
==========

- What about ``__getattr__``, ``__setattr__``, etc?

- Can only have one ``__getattr__`` per class (and ``__setattr__``,
  etc); different behaviours must be multiplexed.

- Implementations cannot easily be shared or reused except through
  inheritance.


Motivation
==========

- We want custom attribute access behaviours.

- We want *reusable* custom attribute access behaviours.

- It would be nice to be able to use them *declaratively*.

- It would also be nice to be able to parameterise the behaviours.

- **Descriptors can do this.**


Descriptors in standard library
===============================

- ``property``

- ``classmethod``

- ``staticmethod``

- ``functools.partialmethod``

- ... and a few more.


Descriptor protocol
===================

A descriptor is a **new-style class** with one or more of the
following methods implemented:

.. code:: python

    def __get__(self, instance, owner):
        return "the value"  # or raise

    def __set__(self, instance, value):
        pass  # "set" the value

    def __delete__(self, instance):
        pass  # "delete" the attribute


- ``self`` is the **descriptor** instance

- ``owner`` is the **class**

- ``instance`` is the **instance** of ``owner`` (or ``None`` if
  accessing via the class object)


Example: type constraints
=========================

.. code:: python

    class TypeConstraint(object):
        def __init__(self, constraint):
            self.constraint = constraint
            self.data = weakref.WeakKeyDictionary()

        def __get__(self, instance, owner):
            return self.data[instance]

        def __set__(self, instance, value):
            if not isinstance(value, self.constraint):
                raise TypeError
            else:
                self.data[instance] = value


Example: type constraints
=========================

- Now we have a *reusable* alternative attribute-access behaviour.
  - Checks type when setting attribute.

- It is also *parameterised* over the ``constraint``.

- Great.  So how do we use it?


Example: type constraints
=========================

To use a descriptor, assign an **instance** of a descriptor class to
a **class** attribute (new-style classes only):

.. code:: python

    class Point(object):
        x = TypeConstraint(numbers.Real)
        y = TypeConstraint(numbers.Real)

        def __init__(self, x, y):
            self.x = x
            self.y = y


Example: type constraints
=========================

- Example on previous slide uses two ``TypeConstraint`` descriptor
  *instances*.

- They are used *declaratively*. Interpret as: "``x``/``y`` is
  *constrained* to ``Real`` numbers."

- We *could* use other kinds descriptors alongside ``x`` and ``y``
  just as easily.
  - With ``__getattr__`` we would have to multiplex
    the behaviours.


Example: ``classmethod``
========================

.. code:: python

    class ClassMethod(object):
        def __init__(self, func):
            self.func = func

        def __get__(self, instance, owner):
            def newfunc(*args, **kwargs):
                return self.func(owner, *args, **kwargs)
            return newfunc

Adapted from class method example:
https://docs.python.org/2/howto/descriptor.html#static-methods-and-class-methods


Example: ``classmethod``
========================

.. code:: python

    class Foo(object):
        @ClassMethod
        def example(cls, *args, **kwargs):
            print(
                "class method called for class {} "
                "with args ({},{})"
                .format(cls, args, kwargs)
            )


Implementation
==============

- In Python since 2.2.  Supported by PyPy, IronPython, Jython?  You
  almost certainly have them in your Python.

- One descriptor *instance* is shared by *all* instances of a class,
  for a descriptor-based attribute.

- Descriptors can be implemented using the C API.


Implementation: when keys collide
=================================

- Descriptors that define ``__get__`` *and* ``__set__`` are called
  *data descriptors*.

- Descriptors only defining ``__get__`` are *non-data descriptors*.

- What's the difference?  Key collision for a descriptor in class
  dictionary and value in instance dictionary.
  - For data descriptors, the descriptor takes precedence.
  - For non-data descriptors, instance dict takes precedence.

- To define a *read-only data descriptor*, implement ``__set__`` and
  raise ``AttributeError``.


Implementation: storing values
==============================

- If using descriptors to store instance data or other values, have
  to decide *where* to store it.

  - Store against the *instance* (in some attribute, by
    convention; using ``id`` of descriptor object can be handy).

  - Store against the *descriptor* object (as in
    ``WeakKeyDictionary`` example).

- I prefer storing in the instance.

- Use case or space/performance considerations may commend a
  particular approach.


Descriptors and metaclasses
===========================

- Remarkable things are possible by combining descriptors with
  *metaclasses*.

- Metaclasses can look for descriptors in the *class dictionary* and
  set up even more advanced behaviours.

- Make sure your use case justifies the complexity!

- No more metaclass details here. Different topic---different talk.

- This is what I did in Elk_.


Elk
===

- Object system for Python inspired by Moose_ for Perl 5.

- Features include: roles, method modifiers, attribute delegation*,
  default values, lazy initialisation*, read-only attributes*,
  required attributes*, type constraints*.

  - \* uses descriptors

- Constructors for free.  Works well with inheritance.
  Comprehensive test suite.


Elk - example
=============

.. code:: python

  class Point(elk.Elk):
      x = elk.ElkAttribute(
        mode='rw', type=numbers.Real, required=True)

      y = elk.ElkAttribute(
        mode='rw', type=numbers.Real, required=True)

  class Point3D(Point):
      z = elk.ElkAttribute(
        mode='rw', type=numbers.Real, required=True)


Elk - example
=============

.. code:: python

  >>> Point()
  Traceback (most recent call last):
    File "<stdin>", line 1, in <module>
    File "elk/meta.py", line 105, in __call__
      if getattr(attrdescs[k], method)(obj, value):
    File "elk/attribute.py", line 182, in init_instance_required
      raise AttributeError('required attribute not provided')
  AttributeError: required attribute not provided


Elk - example
=============

.. code:: python

  >>> Point(x=0, y='wat')
  Traceback (most recent call last):
    File "<stdin>", line 1, in <module>
    File "elk/meta.py", line 105, in __call__
      if getattr(attrdescs[k], method)(obj, value):
    File "elk/attribute.py", line 150, in init_instance_value
      self.__set__(instance, value[0], force=True)
    File "elk/attribute.py", line 199, in __set__
      .format(self._name, self._type)
  TypeError: 'y' attribute must be a <class 'numbers.Real'>


Elk - example
=============

.. code:: python

  >>> p = Point(x=0, y=0)
  >>> p.y = 'wat'
  Traceback (most recent call last):
    File "<stdin>", line 1, in <module>
    File "elk/attribute.py", line 199, in __set__
      .format(self._name, self._type)
  TypeError: 'y' attribute must be a <class 'numbers.Real'>


Elk - example
=============

.. code:: python

  >>> del p.x
  Traceback (most recent call last):
    File "<stdin>", line 1, in <module>
    File "elk/attribute.py", line 42, in wrapped
      return method(self, instance, *args)
    File "elk/attribute.py", line 206, in __delete__
      raise AttributeError('cannot delete required attribute')
  AttributeError: cannot delete required attribute


Elk - ``ElkAttribute``
======================

.. code:: python

    @_key_error_to_attribute_error
    def __get__(self, instance, owner):
        if instance is None:
            return self
        _id = id(self)
        if _id not in instance.__elk_attrs__ \
                and _id in instance.__elk_lazy__:
            self.__set__(
                instance,
                instance.__elk_lazy__[_id](),
                force=True
            )
        return instance.__elk_attrs__[id(self)]


Elk - ``ElkAttribute``
======================

.. code:: python

    def __set__(self, instance, value, force=False):
        if self._mode == 'ro' and not force:
            raise AttributeError('{!r} attr is read-only')
        if self._type is not None \
                and not isinstance(value, self._type):
            raise TypeError(
                '{!r} attribute must be a {!r}'
                .format(self._name, self._type)
            )
        instance.__elk_attrs__[id(self)] = value


Elk - ``ElkAttribute``
======================

.. code:: python

    @_key_error_to_attribute_error
    def __delete__(self, instance):
        if self._required:
            raise AttributeError(
              'cannot delete required attribute')
        del instance.__elk_attrs__[id(self)]


Summary
=======

- We want *reusable* *alternative* attribute-access
  semantics.
  - Many use cases: ORMs, audit, validation etc.
  - ``__getattr__`` and friends don't cut it---poor *reusability*
    and hard to *parameterise*.
  - Descriptors solve these problems!

- Descriptor *protocol* and implementation examples.
  - Looked at a few technical tradeoffs and gotchas.

- You can do useful and powerful things with descriptors.
  - Especially when combined with *metaclasses*.


Resources
=========

- Descriptor HowTo Guide: https://docs.python.org/2/howto/descriptor.html
- Data model reference: https://docs.python.org/2/reference/datamodel.html#implementing-descriptors
- Elk: http://frasertweedale.github.io/elk/
- Moose: https://metacpan.org/module/Moose

.. _Moose: https://metacpan.org/module/Moose
.. _Elk: http://frasertweedale.github.io/elk/
.. _Data model reference: https://docs.python.org/2/reference/datamodel.html#implementing-descriptors
.. _Descriptor HowTo Guide: https://docs.python.org/2/howto/descriptor.html


Thanks for listening
====================

Copyright 2014  Fraser Tweedale

This work is licensed under the Creative Commons Attribution 4.0
International License. To view a copy of this license, visit
http://creativecommons.org/licenses/by/4.0/.

Slides
  https://github.com/frasertweedale/talks/
Email
  ``frase@frase.id.au``
Twitter
  ``@hackuador``


Questions
=========
