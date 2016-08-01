
# sc-haskell

A survey on the topic of sequentially consistent Haskell.

### Inventory of docker images:

First, note that "-dbg" variants count the number of storeLoadBarrier
calls and trace them to the ghc-events trace.

Also, "-opt" builds are supposed to have an optimization where we
"trusted" the Handle and Bytestring uses of IORef (only).

 * v0.2: our first usable dockerized version.  ??more here??

 * v0.3: (Jun 3) Michael updated the LLVM code generator. ??more here??
   Includes the May 20 changes to inline barriers.

 * v0.4: (July 12) we did a lot of benchmarking with this one.
    Several commits.   ??
 
 * v0.5: (July _) This was where we fixed the problem with the non-opt version
   lacking the Handle barriers.  In "v0.5" Handle has barriers.

QUESTION: Our earlier numbers from Nofib were 0.3% geomean slowdown
(out of line barriers), and 0.1% geomean slowdown for inline barriers.

Now [2016.08.01] we're getting slower nofib results and trying to
figure out why.


## Notes:


#### [2016.04.04] {Initial counts for Hackage and stackage}

 * 180 bechmark suites among 1770 Stackage packages.
# haskell-rebuild-base-hello-world

#### [2016.05.15] {Benchmarking script}

* Using the benchmarking script in this repo, 134 Stackage LTS-5.16 repos were found to have at least one benchmark. Of those, 66 ran to successful completion

#### [2016.07.11] {Detective work on bytestring and Sandi}

Suspects:
 
  * Bytestring.append
  * 


.
