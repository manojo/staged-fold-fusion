Staged Fold Fusion
==================

This repo contains an implementation of fold-based fusion using partial evaluation.
The code uses [LMS](http://scala-lms.github.io), or Lightweight
Modular Staging.

Update (27 april 2015): The Scala version has just been bumped. If you have
downloaded this project before this date, and are updating, make sure you do the
same for the LMS dependency as well (i.e. publish-local the latest version).

Important Files
===============

The main files for this project are

  1. [FoldLeft.scala](https://github.com/manojo/staged-fold-fusion/blob/master/src/main/scala/barbedwire/FoldLeft.scala):
  This file contains the implementation of the staged `FoldLeft` API.
  2. [FoldLeftSuite.scala](https://github.com/manojo/staged-fold-fusion/blob/master/src/test/scala/barbedwire/FoldLeftSuite.scala):
  This file contains test cases for `FoldLeft`. These tests generate code, and also
  compile the generated code and run them on dynamic output. The tests programs
  are defined in the `FoldLeftProg` trait.
  Their generation and execution is done in the `FoldLeftSuite` class.
  3. [foldleft.check](https://github.com/manojo/staged-fold-fusion/blob/master/test-out/foldleft.check),
  [partition.check](https://github.com/manojo/staged-fold-fusion/blob/master/test-out/partition.check),
  [reverse-index.check](https://github.com/manojo/staged-fold-fusion/blob/master/test-out/reverse-index.check).
  They contain the generated code for the test cases, along with printed output
  which is the result of running a specific input.

Running the code
================
To run the code, please follow these steps:

  1. Clone the lms repo: `git clone git@github.com:TiarkRompf/virtualization-lms-core.git lms`.
  2. Make sure you are on the `develop` branch: `cd lms; git checkout develop`.
  3. Publish locally: `sbt "publish-local"`.
  4. Clone this here repo in a separate folder: `git clone git@github.com:manojo/staged-fold-fusion.git`.
  5. Profit:
  ```
    $ cd staged-fold-fusion
    $ sbt
    > test-only barbedwire.FoldLeftSuite
  ```

Hope you have fun!
