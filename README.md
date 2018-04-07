# hPAPI

This library provides the Haskell bindings for the Performance API (PAPI) library, <http://icl.cs.utk.edu/papi/>.


Installation
============
This package requires the PAPI C library and kernel modules
to be installed before you get started.

But the binding itself uses a standard Cabal building system
so it can be simply installed with:

  runhaskell Setup.hs configure
  runhaskell Setup.hs build
  runhaskell Setup.hs install

The last command may need to be run as root.

Usage
=====
The basic use of this library is demonstrated by the following example.

  main = do
    [l1,tlb,fp] <- withCounters [papi_l1_dcm, papi_tlb_dm, papi_fp_ins]
                     ioOperationToMeasure
    print (l1, tlb, fp)

Documentation
=============
Documentation is minimal at the moment.  In the mean time,
the original PAPI library documentation may prove useful.

Future Directions
=================
 - The following "advanced" features of PAPI have been left
   out in this first release:
    - Multiplexing
    - Threads
    - Locks
    - Overflow
    - Statistical Profiling

 - Separate GC and mutator counters

 - Integration with GHC's built-in PAPI

 - Improved documentation.

 - Depending on feedback, modules may get reorganized

 - Better handling of errors from the PAPI library
   (currently they just throw IO errors)

 - The PAPI C library is inconsistent on whether EventCode
   is a signed int or an unsigned int.
   This causes minor issues to be fixed.

Comparison with GHC's built-in PAPI
===================================
GHC supports a built-in version of PAPI
(see http://hackage.haskell.org/trac/ghc/wiki/PAPI).
This system is different in a few ways.

First, hpapi allows you to measure specific parts of a program
while GHCI's PAPI only does whole program measurement

Second, since hpapi is a library you can have fine grained
and sophisticated control over how things are measured.
On the other hand GHC's PAPI as part of the RTS gives you less
control, but on the plus side doesn't require any modification
to the program.

Finally, GHC's PAPI splits apart the counts that come from
the garbage collector from those that come from the mutator,
while hpapi combines them.  This is a limitation of hpapi
we hope to correct some time in the future.

It remains to be seen whether it would be worth while
to unify these two systems.

Acknowledgments
===============
Supported, in part, by the National Science Foundation
under grant number CCF-0541364.
