questions:

- can the cockpit browser terminal be resized?

- terminal echo off mode?  where are we at with that?

- Rewind?  where are we at with that?

- tlog-rec-session is setuid.
  I see a commit mentioning "session locking support"
  which manipulates a global lock dir.  Is this the only
  thing that requires setuid?

- reading tap.c, am I correct in saying that tloc-rec can work
  whether or not the process that invokes it is connected to a tty.
  i.e. if there is no tty it will just record all the output from
  the child process (and input, if configured)?



DEMOS:

- telnet nyancat.dakko.us

- tlog-record

- echo -ne '\e[8;16;64t'

General notes
-------------

- forkpty(3) is used to fork a new pty when spawning child shell
