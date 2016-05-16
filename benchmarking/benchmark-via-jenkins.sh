#!/bin/bash
set -xe

RESULTS_DIR=${HOME}/results_backup/benchmark_stackage/${BUILD_TAG}/${NODE_NAME}

./benchmarking/Benchmark.hs
cp -r .bench-build ${RESULTS_DIR}/
