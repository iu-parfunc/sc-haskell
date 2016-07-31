#!/bin/bash

# A temporary script for the purpose of COUNTING the per-getline barriers in the benchmark.

set -xe

rm -f *.eventlog; 

# IMG=parfunc/sc-haskell:v0.4-dbg
# IMG=parfunc/sc-haskell:v0.4-opt-dbg
IMG=parfunc/sc-haskell:v0.5-dbg

docker run -it $IMG ghc --version
stack --docker --docker-image=$IMG bench --benchmark-arguments="--iters 1 from-disk/put-1000_get-1000"

ds *.eventlog
ghc-events show Handle.eventlog | grep Barrier | tail
# [2016.07.31] This is printing "" barriers currently.
# "" of them are already there in the -opt-dbg version.
