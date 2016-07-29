#!/bin/bash

set -xe

rm -f *.eventlog; 

# IMG=parfunc/sc-haskell:v0.4-dbg
IMG=parfunc/sc-haskell:v0.4-opt-dbg

docker run -it $IMG ghc --version
stack --docker --docker-image=$IMG bench --benchmark-arguments="--iters 1 from-disk/put-1000_get-1000"

ds *.eventlog
ghc-events show Handle.eventlog | grep Barrier | tail
