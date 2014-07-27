..
  Copyright 2014  Fraser Tweedale.

  This work is licensed under the Creative Commons Attribution 4.0
  International License. To view a copy of this license, visit
  http://creativecommons.org/licenses/by/4.0/.


***********************************************
Descriptors - a tool for every Python developer
***********************************************

Fraser Tweedale
***************

Me
==

- Developer at Red Hat
- FreeIPA identity management and Dogtag PKI
- Mostly Python and Java at work
- Mostly Haskell at home


Motivation
==========

- Attribute access is an *abstraction*.

- The default attribute access behaviour is often appropriate...

- but sometimes it is useful to *augment* or
  *replace* this default behaviour.

- Such non-default access behaviours should be *re-usable*.

- It should be possible to use non-default access behaviours
  *declaratively*.


Motivation
==========

**Descriptors do this.**


Use cases
=========

- ORMs

- Type* checking or other validation

- Callbacks

- \* Not real types.  See Tony's DjangoCon keynote.


Descriptors you might have used
===============================

- ``property``
- ``classmethod``
- ``staticmethod``
- ``functools.partialmethod``
- ``unittest.mock.PropertyMock``
- ``types.DynamicClassAttribute``


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
            self.map = weakref.WeakKeyDictionary()

        def __get__(self, instance, owner):
            return self.map[instance]

        def __set__(self, instance, value):
            if not isinstance(value, self.constraint):
                raise TypeError
            else:
                self.map[instance] = value


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


Example: ``classmethod``
========================

.. code:: python

    class ClassMethod(object):
        def __init__(self, func):
            self.func = func

        def __get__(self, instance, cls):
            def newfunc(*args, **kwargs):
                return self.func(cls, *args, **kwargs)
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


Implementation notes
====================

- One descriptor *instance* shared by *all* classes, for a
  descriptor-based attribute.

- Descriptors can be implemented using the C API.


Implementation notes
====================

- Descriptors that define ``__get__`` *and* ``__set__`` are called
  *data descriptors*.

- Descriptors only defining ``__get__`` are *non-data descriptors*.

- Difference: data descriptors take precedence over instance
  dictionary, and vice versa.


Implementation notes
====================

- If storing instance data, have to decide where to store it.

- Could store on *instance* (in some attribute, by convention; using
  ``id`` of descriptor object can be handy).

- Could store on *descriptor* (as in ``WeakKeyDictionary`` example).

- I prefer storing in instance; use case or space/performance
  considerations may commend a particular approach.


Descriptors and metaclasses
===========================

- Remarkable things are possible by combining descriptors with
  metaclasses

- Metaclasses can look for descriptors in *class dictionary* and set
  up even more advanced behaviours.

- Make sure your use case justifies the complexity!

- No more details here. Different topic---different talk.

- This is what I did in Elk_.


Elk
===

- Object system for Python inspired by Moose_ for Perl 5.

- Features include: roles, method modifiers, attribute delegation*,
  default values, lazy initialisation*, read-only attributes*,
  required attributes*, type constraints*.

- \* uses descriptors


Elk
===

.. code:: python

  class Point(elk.Elk):
      x = elk.ElkAttribute(mode='rw', type=int)
      y = elk.ElkAttribute(mode='rw', type=int)

      def clear(self):
          self.x = 0
          self.y = 0

  class Point3D(Point):
      z = elk.ElkAttribute(mode='rw', type=int)

      @elk.after('clear')
      def clear_z(self):
          self.z = 0


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


Questions
=========


Thanks for listening
====================

Copyright 2014  Fraser Tweedale.

This work is licensed under the Creative Commons Attribution 4.0
International License. To view a copy of this license, visit
http://creativecommons.org/licenses/by/4.0/.

Slides at https://github.com/frasertweedale/talks/.
