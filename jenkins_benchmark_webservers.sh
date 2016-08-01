#!/bin/bash

# Run this from the directory containing it.

set -xe

ITERS=10

git clone git@github.com:iu-parfunc/FrameworkBenchmarks || echo ok
cd ./FrameworkBenchmarks

COMMIT=`git rev-parse --verify HEAD`

DEST=`hostname -s`_`date "+%s"`

cd deployment/vagrant-production
vagrant up

tests=" wai yesod spock snap "

vagrant ssh -- "cd FrameworkBenchmarks; git remote add fork git@github.com:iu-parfunc/FrameworkBenchmarks" || echo ok

vagrant ssh -- "cd FrameworkBenchmarks; git checkout $COMMIT"
vagrant ssh -- "cd FrameworkBenchmarks; sudo git clean -fxd"

for ((i=0; i < $ITERS; i++)); do 
echo "Running iteration $i"
for test in $tests; do 
  vagrant ssh -- rm -rf FrameworkBenchmarks/results
  mkdir -p ./$DEST/$test

  echo "cd FrameworkBenchmarks; time toolset/run-tests.py --mode benchmark --test $test" | vagrant ssh
  vagrant ssh -- cp -a FrameworkBenchmarks/results /vagrant/$DEST/$test/
done
done

DESTDIR="$HOME/results_backup/TechEmpower/"

if ! [ -e $DESTDIR ]; then
    mkdir -p $DESTDIR
fi

# if ! [ "$BUILD_TAG" == "" ]; then DESTDIR+="$BUILD_TAG/"; fi
if ! [ "$BUILD_NUMBER" == "" ]; then DESTDIR+="jenkins_build${BUILD_NUMBER}/"; fi

DESTDIR+="$DEST/"
mkdir -p "$DESTDIR"

rsync -rplt ./$DEST/ $DESTDIR/
