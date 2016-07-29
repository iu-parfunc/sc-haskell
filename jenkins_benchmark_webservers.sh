#!/bin/bash

# Run this from the directory containing it.

set -xe

git clone git@github.com:iu-parfunc/FrameworkBenchmarks || echo ok
cd ./FrameworkBenchmarks

COMMIT=`git rev-parse --verify HEAD`

DEST=`hostname -s`_`date "+%s"`

# [crest-team@c-swarm ~/local/FrameworkBenchmarks/deployment/vagrant-production] (sc-v0.4)
# $ vagrant ssh -- "cd FrameworkBenchmarks; ./test"

cd deployment/vagrant-production
vagrant up


vagrant ssh -- "cd FrameworkBenchmarks; git remote add fork git@github.com:iu-parfunc/FrameworkBenchmarks" || echo ok

vagrant ssh -- "cd FrameworkBenchmarks; git checkout $COMMIT"
vagrant ssh -- "cd FrameworkBenchmarks; git clean -fxd"

vagrant ssh -- "cd FrameworkBenchmarks; time toolset/run-tests.py --mode benchmark --test snap"
vagrant ssh -- cp -a FrameworkBenchmarks/results /vagrant/$DEST/

DESTDIR="$HOME/results_backup/"

if ! [ -e $DESTDIR ]; then
    mkdir -p $DESTDIR
fi

if ! [ "$BUILD_TAG" == "" ]; then
    DESTDIR+="$BUILD_TAG/"
fi

DESTDIR+="$DEST/"
mkdir -p "$DESTDIR"

rsync -rplt ./$DEST/ $DESTDIR/
