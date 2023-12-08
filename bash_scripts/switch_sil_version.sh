#!/bin/bash

set -e

BASEDIR="$(dirname -- "$(dirname -- "$(realpath -- "$0")")")"

cd $BASEDIR/silicon
git checkout -B silVersionBenchmarkingBranch $1
cd $BASEDIR
sbt compile
