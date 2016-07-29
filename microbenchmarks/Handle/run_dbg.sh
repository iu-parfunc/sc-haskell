#!/bin/bash

set -xe

rm -f *.eventlog; 

IMG=parfunc/sc-haskell:v0.4-dbg

docker run -it $IMG ghc --version
stack --docker --docker-image=$IMG bench --benchmark-arguments="--iters 1"

ds *.eventlog
ghc-events show Handle.eventlog | grep Barrier | tail
