#!/bin/bash
set -xe

if [ "${BUILD_TAG}" == "" ]; then
  BUILD_TAG="jenkins-unknown"
fi

if [ "${NODE_NAME}" == "" ]; then
  NODE_NAME="node-unknown"
fi

WORK_DIR=`pwd`
TMP_WORK_DIR=`mktemp -d /tmp/workdirXXXXXXXXXX`
RESULTS_DIR=${HOME}/results_backup/benchmark_stackage/${BUILD_TAG}/${NODE_NAME}

# /opt/modules/Modules/3.2.10/bin/modulecmd bash add llvm
# eval `/opt/modules/Modules/3.2.10/bin/modulecmd bash add llvm`
# C_INCLUDE_PATH=/usr/include/x86_64-linux-gnu:/usr/include:$C_INCLUDE_PATH
# LIBRARY_PATH=/usr/lib/x86_64-linux-gnu:/usr/lib:$LIBRARY_PATH
# LD_LIBRARY_PATH=/usr/lib/x86_64-linux-gnu:/usr/lib:$LD_LIBRARY_PATH
# PKG_INCLUDE_PATH=/usr/lib/x86_64-linux-gnu/pkgconfig:/usr/lib/pkgconfig:$PKG_CONFIG_PATH
# PATH=$PATH:/usr/bin

cp -a $WORK_DIR/. $TMP_WORK_DIR/
cd $TMP_WORK_DIR

if [[ "${NODE_NUMBER}" != "" && "${TOTAL_NODES}" != "" ]]; then
  ./Benchmark.hs --slice=${NODE_NUMBER} --numSlices=${TOTAL_NODES}
else
  echo "WARNING: Not running in parallel!"
  ./Benchmark.hs
fi

mkdir -p ${RESULTS_DIR}
cp -a bench-res ${RESULTS_DIR}/
