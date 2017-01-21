..
  Copyright 2016  Red Hat, Inc.

  This work is licensed under the Creative Commons Attribution 4.0
  International License. To view a copy of this license, visit
  http://creativecommons.org/licenses/by/4.0/.


*****
Intro
*****

  
==

.. raw:: latex

  \begin{center}
  \includegraphics[width=.6\columnwidth]{Laptop-hard-drive-exposed.jpg}
  \end{center}
  \tiny

CC BY-SA 3.0 https://commons.wikimedia.org/wiki/File:Laptop-hard-drive-exposed.jpg


  
==

.. raw:: latex

  \begin{center}
  \def\svgwidth{.5\paperheight}
  \input{identity-secure-ARTIFACT.pdf_tex}
  \end{center}

  
==

.. raw:: latex

  \begin{center}
  \def\svgwidth{.6\columnwidth}
  \input{USB_Icon-ARTIFACT.pdf_tex}
  \end{center}


****
Demo
****

Tang
====

- Simple provisioning of encryption for secrets

- Automated decryption when Tang server is available
  - secret is *bound to network*

- Secret **never** leaves the client


Tang - assumptions
==================

- Tang server only accessible from "secure network"

- Secrets and keys are safe in client memory


Diffie-Hellman exchange
=======================

- Key agreement protocol

- Alice and Bob agree on a *shared secret*

- Eve cannot learn shared key


Integrated Encryption Scheme
============================

- Encryption protocol based on DH

- Derive *symmetric key* from shared secret

- Alice encrypts a message to Bob's public key; sends it

- Bob can decrypt the message, Eve cannot


McCallum-Relyea exchange
========================

- Encryption protocol based on IES
  - due to Nathaniel McCallum and Robert Relyea:
    https://marc.info/?m=144173814525805

- Alice encrypts a message to Bob's public key; **doesn't send it**

- To decrypt, Alice asks Bob to encrypt an *ephemeral key*
  - uses reply to recover secret

- Eve cannot decrypt the message ***and neither can Bob!***


.. raw:: latex

  \end{frame}

  \begin{frame}[plain]
  \begin{columns}
  \column{\dimexpr\paperwidth}
    \includegraphics[height=\paperheight]{caution.jpg}
  \end{columns}


McCallum-Relyea - parameters
============================

- cyclic group :math:`G` of order :math:`n`, with *hard problem*
  - :math:`ℤ^*_p` (discrete log)
  - elliptic curve :math:`E(𝔽_q)` (point factorisation)

- generator :math:`g \in G`

- key derivation function :math:`KDF`

- symmetric encryption algorithm :math:`Enc`

- message :math:`m` to be encrypted


McCallum-Relyea - encryption
============================

.. raw:: latex

  \begin{center}
  \def\arraystretch{1.5}%
  \begin{tabular}{ l l }
    \multicolumn{1}{c}{Client} & \multicolumn{1}{c}{Server} \\ \hline
    $A \in_R [1, n-1]$ & $B \in_R [1, n-1]$ \\
    & $b \gets g^B$ \\
    \multicolumn{2}{c}{$ \gets b $} \\
    $ K \gets KDF(b^A) = KDF(g^{AB}) $ & \\
    $ a \gets g^A,~~~c \gets Enc(K, m) $ & \\
    $ \varnothing \gets A, K $ & \\
  \end{tabular}
  \end{center}


McCallum-Relyea - decryption
============================

.. raw:: latex

  \begin{center}
  \def\arraystretch{1.5}%
  \begin{tabular}{ l l }
    \multicolumn{1}{c}{Client} & \multicolumn{1}{c}{Server} \\ \hline
    $X \in_R [1, p-1]$ & \\
    $ x \gets a \cdot g^X = g^A \cdot g^X $ & \\
    \multicolumn{2}{c}{$ x \to $} \\
    & $x' \gets x^B = g^{AB} \cdot g^{XB} $ \\
    \multicolumn{2}{c}{$ \gets x' $} \\
    $ K \gets KDF(x' \cdot (b^X)^{-1}) $ & \\
    $ ~~~ = KDF(g^{AB} \cdot g^{XB} \cdot g^{-XB}) = KDF(g^{AB}) $ & \\
    $ m \gets Enc^{-1}(K, c) $ & \\
  \end{tabular}
  \end{center}


Tang - implementation
=====================

- Server-side daemon and *Clevis pin*

- C

- Extensive test suite

- Small and fast (>30k req/sec)


Tang - protocol
===============

- UDP

- ASN.1 (DER)

- No encryption (none needed)

- Trust On First Use (TOFU)
  - Signed messages allow key rotation
  - OOB fingerprint validation / key pinning are possibilities


Tang - threats and caveats
==========================

- MitM during provisioning

- Tang server is DoS target

- Good entropy needed for ephemeral key :math:`X`

- Quantum computing


********************
Mission accomplished
********************

***
???
***


To what other things can we bind secrets?
=========================================

- Trusted Platform Module (TPM)
- Smartcard
- Bluetooth LE beacon
- Biometrics
- "Master unlock key"


Unlock policy
=============

- Security is not binary

- Policy should be driven by business needs, *not technology*

- How can we support arbitrarily complex unlock policy? e.g.

  - :math:`stage1 \gets S \subset \{ pwd, tang, smartcard, fingerprint \}, |S| \ge 2`
  - :math:`stage2 \gets \{ stage1, tpm \}`
  - :math:`unlock \gets S \subset \{ stage2, pwd \}, |S| \ge 1`


Shamir's Secret Sharing
=======================

- :math:`k` points describe a polynomial of degree :math:`k - 1`

- Free coefficient :math:`\gets` secret, other coefficients
  :math:`\gets_R`

- Distribute :math:`n` points (:math:`n \ge k, x \ne 0`)

- Given :math:`k` points, compute *Lagrange polynomial*
  - secret :math:`\gets f(0)`


Shamir's Secret Sharing
=======================

.. raw:: latex

  \begin{center}
  \def\svgwidth{.8\columnwidth}
  \input{Lagrange_polynomial-ARTIFACT.pdf_tex}
  \end{center}
  \tiny

CC BY-SA 3.0 https://en.wikipedia.org/wiki/File:Lagrange_polynomial.svg


****
Demo
****


Clevis
======

- Client-side, pluggable key management based on SSS
- *pins* (plugins)
  - tang, password, ...

- JSON configuration
- C; minimal dependencies (openssl, libjansson)


History
=======

- Feb '15: *Deo* project begins (*δεω, to bind*)

  - Used TLS for privacy and X.509 encryption cert (*complexity!*)

  - Server decrypts and returns secret (thus learning it; *bad!*)

- Sep '15: McCallum-Relyea discovered; rewrite begins

- Dec '15: Project split into Tang and Clevis


LUKS integration
================

- *Linux Unified Key Setup*

- LUKS (v1) integration: **Tang only**
  - `LUKSMeta <https://github.com/latchset/luksmeta>`_ library

- LUKS2 (future)
  - Designed for extensibility
  - Full Clevis support (hopefully!)


USBGuard integration
====================

- Automatic encryption/decryption of USB storage media

- Allow *only* Tang-provisioned volumes to be accessed

- Can't be accessed outside network perimeter


Ongoing development
===================

- Key rotation

- Audit logging


Stuff I wish I had time to do right now
=======================================

- TPM Clevis pin

- Let's Encrypt integration (encrypt private keys)
  - Blog post on Apache integration: https://is.gd/hQcpuM


Availability
============

- Fedora 24
  - COPR (unofficial package repo): ``npmccallum/tang``

- Source code (GPLv3+)
  - https://github.com/latchset/tang
  - https://github.com/latchset/clevis


You can help!
=============

- Crypto / protocol / code review

- Try it out!

- Tell us your use cases

- Contribute Clevis pins


Fin
===

.. raw:: latex

  \begin{columns}

    \begin{column}{.4\textwidth}
      \includegraphics[width=1.2\textwidth]{clevis.jpg}
    \end{column}

    \begin{column}{.6\textwidth}

      \setlength{\parskip}{.5em}

      { \centering

      \input{cc-by-ARTIFACT.pdf_tex}

      \copyright~2016  Red Hat, Inc.

      { \scriptsize
      Except where otherwise noted this work is licensed under
      }
      { \footnotesize
      \textbf{http://creativecommons.org/licenses/by/4.0/}
      }

      }

      \begin{description}
      \item[Blog]
      \texttt{blog-ftweedal.rhcloud.com}
      \item[Email]
      \texttt{ftweedal@redhat.com}
      \item[Twitter]
      \texttt{@hackuador}
      \end{description}
    \end{column}

  \end{columns}
