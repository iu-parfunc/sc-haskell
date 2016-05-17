#!/bin/bash
set -xe

if [ "${BUILD_TAG}" == "" ]; then
  BUILD_TAG="jenkins-unknown"
fi

if [ "${NODE_NAME}" == "" ]; then
  NODE_NAME="node-unknown"
fi

RESULTS_DIR=${HOME}/results_backup/benchmark_stackage/${BUILD_TAG}/${NODE_NAME}

module add llvm

./Benchmark.hs
mkdir -p ${RESULTS_DIR}
cp -r .bench-res ${RESULTS_DIR}/
