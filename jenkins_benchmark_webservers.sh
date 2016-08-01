#!/bin/bash

# Run this from the directory containing it.

set -xe

ITERS=10

git clone git@github.com:iu-parfunc/FrameworkBenchmarks || echo ok
cd ./FrameworkBenchmarks

# Which commit in FrameworkBenchmarks depends on which mode we're in:
if [ "$VARIANT" == stock-ghc ]; then
    COMMIT=180d44e1064abc6fe8c703b05e065c0564e6ee05
elif [ "$VARIANT" == sc-v0.4 ]; then
    # This should test all four Haskell implementations with SC GHC v0.4:
    COMMIT=79eaca80914499230ea4b1b61a38af6d09187803
elif [ "$VARIANT" == HEAD ]; then
    COMMIT=`git rev-parse --verify HEAD`
else
    echo "ERROR: VARIANT env var unset.  Should be stock-ghc, sc-v0.4, or HEAD "
    exit 1
fi

# Parochial concerns.  We symlink to this, but on some worker machines it may not exist:
mkdir -p $HOME/local/.vagrant.d

DEST=`hostname -s`_`date "+%s"`

git checkout $COMMIT
cd deployment/vagrant-production

hostname
vagrant --version
VBoxManage list vms
VBoxManage list runningvms
vagrant up

tests=" wai yesod spock snap "

# HACK: We haven't stopped the vagrant provisioner from pointing this
# repository BACK at the parent of our fork.  We let it happen and
# then force it back:
vagrant ssh -- "cd FrameworkBenchmarks; git remote add fork git@github.com:iu-parfunc/FrameworkBenchmarks" || echo ok
vagrant ssh -- "cd FrameworkBenchmarks; git fetch fork"
vagrant ssh -- "cd FrameworkBenchmarks; git checkout $COMMIT"
vagrant ssh -- "cd FrameworkBenchmarks; sudo git clean -fxd"

# Set up final results destination:
# ----------------------------------------
DESTDIR="$HOME/results_backup/TechEmpower/$VARIANT/"

if ! [ -e $DESTDIR ]; then
    mkdir -p $DESTDIR
fi

# if ! [ "$BUILD_TAG" == "" ]; then DESTDIR+="$BUILD_TAG/"; fi
if ! [ "$BUILD_NUMBER" == "" ]; then DESTDIR+="jenkins_build${BUILD_NUMBER}/"; fi

DESTDIR+="$DEST/"
mkdir -p "$DESTDIR"
# ----------------------------------------

for ((i=0; i < $ITERS; i++)); do
  set +x
  echo "Running webserver benchmarks, iteration $i"
  echo "------------------------------------------"
  echo ""
  set -x
  for test in $tests; do 
    vagrant ssh -- rm -rf FrameworkBenchmarks/results
    mkdir -p ./$DEST/$test

    echo "cd FrameworkBenchmarks; time toolset/run-tests.py --mode benchmark --test $test" | vagrant ssh

    # We don't worry about collissions here, because there are time stamps:
    vagrant ssh -- cp -a FrameworkBenchmarks/results /vagrant/$DEST/$test/
    # vagrant ssh -- rsync -vrplt FrameworkBenchmarks/results /vagrant/$DEST/$test/

    # Flush to the central location on the host:
    rsync -rplt ./$DEST/ $DESTDIR/
    
  done
done

vagrant halt
