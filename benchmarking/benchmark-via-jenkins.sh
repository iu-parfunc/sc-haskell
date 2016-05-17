#!/bin/bash
set -xe

if [ "${BUILD_TAG}" == "" ]; then
  BUILD_TAG="jenkins-unknown"
fi

if [ "${NODE_NAME}" == "" ]; then
  NODE_NAME="node-unknown"
fi

RESULTS_DIR=${HOME}/results_backup/benchmark_stackage/${BUILD_TAG}/${NODE_NAME}

eval `/opt/modules/Modules/3.2.10/bin/modulecmd bash add llvm`
C_INCLUDE_PATH=/usr/include/x86_64-linux-gnu:/usr/include:$C_INCLUDE_PATH
LIBRARY_PATH=/usr/lib/x86_64-linux-gnu:/usr/lib:$LIBRARY_PATH
LD_LIBRARY_PATH=/usr/lib/x86_64-linux-gnu:/usr/lib:$LD_LIBRARY_PATH
PKG_INCLUDE_PATH=/usr/lib/x86_64-linux-gnu/pkgconfig:/usr/lib/pkgconfig:$PKG_CONFIG_PATH
PATH=$PATH:/usr/bin

./Benchmark.hs
mkdir -p ${RESULTS_DIR}
cp -r .bench-res ${RESULTS_DIR}/
