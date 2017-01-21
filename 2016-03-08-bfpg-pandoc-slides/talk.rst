*****
Intro
*****

Motivation
==========

.. role:: latex(raw)
   :format: latex

- I do technical presentations

- I want nice looking presentations

- I want to focus on slide content---not formatting!


Overview
========

- *My* slide toolchain

- Examples; live slide hacking

- Pandoc internals

- *Not* a :latex:`\LaTeX{}` tutorial


Toolchain
=========

- Write slides in *ReStructuredText* format

- Fall back to :latex:`{\LaTeX}` if/when needed

- Convert to PDF slideshow using *Pandoc*


ReStructuredText
================

- ReST, ``*.rst``
- Lightweight markup
- Originated from Python Docutils
  - predates Markdown


Pandoc
======

- "Universal document converter"
- Haskell
- GPLv2+


Pandoc - running
================

File ``./build.sh``:

.. code:: bash

  #!/bin/sh
  pandoc talk.rst --to=beamer -o slides.pdf \
    --template=my.beamer \
    --highlight-style=pygments \
    --variable=title:"Dr. Slidelove" \
    -V subtitle:"or: How I Learned to Stop Worrying and Love the Beamer" \
    -V author:"Fraser Tweedale" \
    -V date:"\\today" \
    ...

Pandoc - templates
==================

- Each Pandoc *writer* has a default *template*

- ``pandoc --print-default-template=beamer > ...``

- ``pandoc --template="my.beamer"``

- Upgrades


********
Examples
********

Basic formatting (source)
=========================

.. code:: rst

  Basic formatting (output)
  =========================

  .. comment (ignored)

  - **boldface**
  - *emphasis*
  - ``monospace``
    - foo
    - bar


Basic formatting (output)
=========================

.. comment (ignored)

- **boldface**
- *emphasis*
- ``monospace``
  - foo
  - bar


Hyperlinks (source)
===================

.. code:: rst

  Hyperlinks (output)
  ===================

  - https://foo.com/

  - `Embedded link <https://bar.com/>`_

  - `External link`_

  .. _External link: https://baz.com/

Hyperlinks (output)
===================

- https://foo.com/

- `Embedded link <https://bar.com/>`_

- `External link`_

.. _External link: https://baz.com/


Incremental lists (source)
==========================

.. code:: rst

  Incremental lists (output)
  ==========================

    1. one
    #. at
    #. a
    #. time


Incremental lists (output)
==========================

  1. one
  #. at
  #. a
  #. time


Code (source)
=============

.. code:: rst

  Code (output)
  =============

  - Syntax highlighting via Pygments

  - >300 supported languages
    - http://pygments.org/docs/lexers/

  .. code:: haskell

    -- |
    -- prop> rev [x] == [x]
    -- prop> rev (xs ++ ys) == rev ys ++ rev xs
    rev :: [a] -> [a]
    rev = foldl (flip (:)) []


Code (output)
=============

- Syntax highlighting via Pygments

- >300 supported languages
  - http://pygments.org/docs/lexers/

.. code:: haskell

  -- |
  -- prop> rev [x] == [x]
  -- prop> rev (xs ++ ys) == rev ys ++ rev xs
  rev :: [a] -> [a]
  rev = foldl (flip (:)) []


Math - standalone (source)
==========================

.. code:: rst

  Math - standalone (output)
  ==========================

  .. math::

    x = a_0 + \cfrac{1}{a_1
            + \cfrac{1}{a_2
            + \cfrac{1}{a_3 + \cfrac{1}{a_4} } } }

Math - standalone (output)
==========================

.. math::

  x = a_0 + \cfrac{1}{a_1
          + \cfrac{1}{a_2
          + \cfrac{1}{a_3 + \cfrac{1}{a_4} } } }


Math - inline (source)
======================

.. code:: rst

  Math - inline (output)
  ======================

  - ``math`` role can be used inline

  - Like so: :math:`{n! \over k!(n-k)!} = {n \choose k}`

Math - inline (output)
======================

- ``math`` role can be used inline

- Like so: :math:`{n! \over k!(n-k)!} = {n \choose k}`


Raw :latex:`{\LaTeX}` - standalone (source)
===========================================

.. code:: rst

  Raw :latex:`{\LaTeX}` - standalone (output)
  ===========================================

  .. raw:: latex

    \centering
    \begin{tabular}{|r|l|}
      \hline
      7C0         & hexadecimal \\
      3700        & octal       \\ \cline{2-2}
      11111000000 & binary      \\ \hline \hline
      1984        & decimal     \\ \hline
    \end{tabular}

Raw :latex:`{\LaTeX}` - standalone (output)
===========================================

.. raw:: latex

  \centering
  \begin{tabular}{|r|l|}
    \hline
    7C0         & hexadecimal \\
    3700        & octal       \\ \cline{2-2}
    11111000000 & binary      \\ \hline \hline
    1984        & decimal     \\ \hline
  \end{tabular}


Raw :latex:`{\LaTeX}` - inline (source)
=======================================

.. code:: rst

  .. role:: latex(raw)
     :format: latex

  Raw :latex:`{\LaTeX}` - inline (output)
  =======================================

  - Water = :latex:`H\textsubscript{2}O`
  - :latex:`{\LaTeX} is \textnumero 1 \checkmark`

Raw :latex:`{\LaTeX}` - inline (output)
=======================================

.. role:: latex(raw)
   :format: latex

- Water = :latex:`H\textsubscript{2}O`
- :latex:`{\LaTeX} is \textnumero 1 \checkmark`


Images (source)
===============

.. code:: rst

  Images (output)
  ===============

  .. image:: file_extensions.png
     :width: 40%
     :align: center

  CC BY-NC 2.5 https://xkcd.com/1301/


Images (output)
===============

.. image:: file_extensions.png
   :width: 40%
   :align: center

CC BY-NC 2.5 https://xkcd.com/1301/


Unicode
=======

- ``\usepackage[utf8]{inputenc}`` (Pandoc default)
  - map Unicode characters to arbitrary :latex:`\LaTeX{}`
  - ``\DeclareUnicodeCharacter{2200}{$\forall$}``

- ``\usepackage[utf8x]{inputenc}`` (edit template)
  - loads a library of many predefined mappings

- :latex:`{Lua\LaTeX}` supports direct UTF-8 input
  - some "math" characters missing from text environment


Beamer - title page
===================

- Pandoc variables
  - ``title``, ``subtitle``, ``author``, ``date``

- Can use raw :latex:`\LaTeX{}` in values
  - ``pandoc ... -V date:"\\today"``

- Automatically added iff ``title`` variable is defined


Beamer - appearance
===================

- Use serif typeface for math
  - ``\usefonttheme[onlymath]{serif}``

- Colour hyperlinks
  - ``\hypersetup{colorlinks,linkcolor=,urlcolor=purple}``

- Include a logo
  - ``\logo{\includegraphics[height=0.5cm]{logo.png}}``

- Aspect ratio class option
  - ``aspectratio={43,169,...}``


Beamer - themes
===============

- Beamer has *themes* (layout) and *colour themes*

- https://www.hartwork.org/beamer-theme-matrix/

- Pandoc variables: ``theme``, ``colortheme``


Speaker notes (source)
======================

.. code:: rst

  Speaker notes (output)
  ======================

  - This slide has speaker notes
  - Compile with class option ``notes={show,only}``

  .. class:: notes

    - This is a speaker note
    - Don't forget to mention X

Speaker notes (output)
======================

- This slide has speaker notes
- Compile with class option ``notes={show,only}``

.. class:: notes

  - This is a speaker note
  - Don't forget to mention X


Other approaches
================

- Other markup formats

- Raw Beamer
  - with great power comes... more boilerplate!

- HTML+JS slideshows


****************
Pandoc internals
****************

.. Notes:

  - pandoc-types/Text/Pandoc/Definition.hs (Image)
  - pandoc/src/Text/Pandoc/Readers/RST.hs
  - pandoc/src/Text/Pandoc/Writers/LaTeX.hs



Resources
=========

- http://docutils.sourceforge.net/rst.html
- https://en.wikibooks.org/wiki/LaTeX
- https://tex.stackexchange.com/
- https://ctan.org/pkg/beamer
- http://pandoc.org/


Fin
===

Copyright 2016  Fraser Tweedale

This work is licensed under the Creative Commons Attribution 4.0
International License. To view a copy of this license, visit
http://creativecommons.org/licenses/by/4.0/.

Slides
  https://speakerdeck.com/frasertweedale
Twitter
  `@hackuador`_
Email
  ``ftweedal@redhat.com``

.. _@hackuador: https://twitter.com/hackuador
