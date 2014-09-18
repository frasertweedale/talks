..
  Copyright 2014  Fraser Tweedale.

  This work is licensed under the Creative Commons Attribution 4.0
  International License. To view a copy of this license, visit
  http://creativecommons.org/licenses/by/4.0/.


***************
Haskell on PaaS
***************

Fraser Tweedale
***************


Disclosure
==========

- I work for Red Hat.
- Red Hat is a PaaS provider (OpenShift Online).


What is PaaS?
=============

- *Platform as a Service*

- Software execution environment

- Don't think about hardware / OS

- Don't think (much) about network
  - DNS handled for you
  - SSL/TLS termination
  - Auto-scaling (depending on provider/platform)

- dev/stage/prod environments are easy


Who is PaaS?
============

- OpenShift (Red Hat) †‡
- Heroku †
- CloudFoundry ‡
- FP Haskell Centre (FP Complete) ‡
- ...various others.
- †  free tier available
- ‡  runs on free software i.e. you can run an instance yourself


Haskell on OpenShift
====================

- Community cartridges
  - GHC 7.8
  - Variants for major frameworks (Snap, Yesod, scotty, HappStack)
  - Manifest URLs: http://www.haskell.org/haskellwiki/Web/Cloud
  - Currently a bit broken (I have submitted a PR to fix)

- Manage using ``rhc`` command-line tool.


Haskell on OpenShift
====================

::

  % rhc app create parks \
    http://www.accursoft.com/cartridges/snap.yml \
    postgresql-8.4 \
    --from-code https://github.com/bfpg/brisparks.info.git


Haskell on Heroku
=================

- Supported via "buildpack".
  - https://github.com/begriffs/heroku-buildpack-ghc

- Manage using ``heroku`` command line tool.

- CloudFoundry *appears* to be similar, but I haven't tried it.

Haskell on Heroku
=================

::

  % echo 'web: cabal run -- -p $PORT' > Procfile
  % git add Procfile ; git commit -m 'add Procfile'
  % heroku create --stack=cedar \
    --buildpack \
    https://github.com/begriffs/heroku-buildpack-ghc.git
  % git push heroku master


Haskell on FP Application Server/Keter
======================================

- The only Haskell-centric PaaS (that I know of).
  - So it might be nice...?

- There is no free tier #sadface

- Haven't tried it...
  - but I would love to hear about your experiences if you have/do!


Configuring apps for PaaS
=========================

- Most PaaSen have app configuration in env vars.
  - Differences between providers.
  - More services (databases, metrics, etc) = more config.
  - I'm chipping away on a small library to abstract (some of) this.

- Deployment:
  - Some PaaS run your app according to some convention (e.g.
    OpenShift)
  - Some PaaS require you to tell it how to run your app (e.g.
    Heroku, via ``Procfile``)


Pain points
===========

- Cabal hell.

- Low disk space
  - GHC + Haskell frameworks take up several hundred MB.
  - I have done some work on OpenShift cartridges to improve the
    situation.
  - I have some more ideas.
  - Pay $$$ for more disk.

- Low memory
  - e.g. can't build ``haskell-src-exts`` on small OpenShift gear
  - Solution: ship more cartridge variants with more libs?
  - Pay $$$ for more memory.


Future
======

- Stackage
  - Infrastructure to create stable builds of complete package sets.
  - OpenShift cartridges seem to be moving to Stackage.

- Docker
  - Easier to set up container images.
  - Layered images = shared base images, smaller framework/app
    images.
  - Lots of Docker hosting providers already.
  - OpenShift v3 is embracing Docker.

- First-class support for Haskell in popular PaaS providers?
  - Hopefully!  FP Complete seem to be on their own, for now.


Summary
=======

- PaaS is cool.
- Go play.
- Help make Haskell on PaaS a better experience.


Resources
=========

- PaaS info on HaskellWiki: http://www.haskell.org/haskellwiki/Web/Cloud
- OpenShift Online: https://www.openshift.com/
- Haskell on OpenShift blog post by Kate Miller: http://is.gd/7eRZDY
- OpenShift cartridge development: https://github.com/accursoft/Haskell-Cloud
- Heroku: https://www.heroku.com/
- Heroku buildpack development: https://github.com/begriffs/heroku-buildpack-ghc


Thanks for listening
====================

Copyright 2014  Fraser Tweedale

This work is licensed under the Creative Commons Attribution 4.0
International License. To view a copy of this license, visit
http://creativecommons.org/licenses/by/4.0/.

Slides
  https://github.com/frasertweedale/talks
Email
  ``frase@frase.id.au``
Twitter
  ``@hackuador``
