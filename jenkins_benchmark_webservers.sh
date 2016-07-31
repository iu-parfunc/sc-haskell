#!/bin/bash

# Run this from the directory containing it.

set -xe

ITERS=10

git clone git@github.com:iu-parfunc/FrameworkBenchmarks || echo ok
cd ./FrameworkBenchmarks

# Which commit in FrameworkBenchmarks depends on which mode we're in:
if [ "$VARIANT" == stock-ghc ]; then
    COMMIT=180d44e1064abc6fe8c703b05e065c0564e6ee05
elif [ "$VARIANT" == stock-ghc ]; then
    # This should test all four Haskell implementations with SC GHC v0.4:
    COMMIT=79eaca80914499230ea4b1b61a38af6d09187803
elif [ "$VARIANT" == HEAD ]; then
    COMMIT=`git rev-parse --verify HEAD`
else
    echo "ERROR: VARIANT env var unset.  Should be stock-ghc, sc-v0.4, or HEAD "
    exit 1
fi

DEST=`hostname -s`_`date "+%s"`

cd deployment/vagrant-production
vagrant up

tests=" wai yesod spock snap "

vagrant ssh -- "cd FrameworkBenchmarks; git remote add fork git@github.com:iu-parfunc/FrameworkBenchmarks" || echo ok

vagrant ssh -- "cd FrameworkBenchmarks; git checkout $COMMIT"
vagrant ssh -- "cd FrameworkBenchmarks; sudo git clean -fxd"

for ((i=0; i < $ITERS; i++)); do 
echo "Running iteration $i"a
for test in $tests; do 
  vagrant ssh -- rm -rf FrameworkBenchmarks/results
  mkdir -p ./$DEST/$test

  echo "cd FrameworkBenchmarks; time toolset/run-tests.py --mode benchmark --test $test" | vagrant ssh
  vagrant ssh -- cp -a FrameworkBenchmarks/results /vagrant/$DEST/$test/
done
done

DESTDIR="$HOME/results_backup/TechEmpower/$VARIANT/"

if ! [ -e $DESTDIR ]; then
    mkdir -p $DESTDIR
fi

if ! [ "$BUILD_TAG" == "" ]; then
    DESTDIR+="$BUILD_TAG/"
fi

DESTDIR+="$DEST/"
mkdir -p "$DESTDIR"

rsync -rplt ./$DEST/ $DESTDIR/

vagrant halt
