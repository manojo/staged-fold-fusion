Staged Fold Fusion
==================

[![Build Status](https://travis-ci.org/manojo/staged-fold-fusion.svg?branch=master)](https://travis-ci.org/manojo/staged-fold-fusion)

This repo contains an implementation of fold-based fusion using partial evaluation.
The code uses [LMS](http://scala-lms.github.io), or Lightweight
Modular Staging. You may wish to read the [Scala '15](http://lampwww.epfl.ch/~hmiller/scala2015/)
publication of this work [here](http://infoscience.epfl.ch/record/209021/files/p41-jonnalagedda.pdf).
You can also take a look at slides [here](http://lampwww.epfl.ch/~hmiller/scala2015/slides/fold_based_fusion.pdf).

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

  1. Clone this here repo in a separate folder: `git clone git@github.com:manojo/staged-fold-fusion.git`.
  2. Profit:
  ```
    $ cd staged-fold-fusion
    $ sbt
    > test-only barbedwire.FoldLeftSuite
  ```

Hope you have fun!

Using the code in your project
==============================

Add the following lines to your `sbt` build to start using:

    libraryDependencies += "com.github.manojo" % "staged-fold-fusion
    _2.11" % "0.1-SNAPSHOT"
    resolvers += Resolver.sonatypeRepo("snapshots")

LMS Coding conventions
======================

Here are some basic LMS conventions I use when writing LMS code:

  * Each new `Ops` gets its own file
  * An `Exp` trait only mixes other Exp traits that are not `Opt` trait
  * `Opt` traits are mixed in at a later stage. If you are often going to
    use one, create an `Opt` trait for your `Ops` which mixes the relevant
    `Opt` traits. `Fat` optimizations should not be mixed in with the classical
    `Opt` traits, but used independently.
  * If I use a certain feature (ex Option) in my current trait, I mix it in
    explicitly, even if some other trait I'm using has it already. Serves for
    documentation purposes. Of course, for really trivial stuff it's not necessary
    to do so.

History
=======

Update (27 april 2015): The Scala version has just been bumped. If you have
downloaded this project before this date, and are updating, make sure you do the
same for the LMS dependency as well (i.e. publish-local the latest version).

Update (7 september 2015): You no longer need to publish-local!

Update (11 september 2015): You can now re-use this code in a sane manner

