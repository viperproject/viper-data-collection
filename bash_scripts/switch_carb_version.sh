#!/bin/bash

set -e

BASEDIR="$(dirname --  "$(dirname -- "$(realpath -- "$0")")")"

cd $BASEDIR/carbon
git checkout -B carbVersionBenchmarkingBranch $1
cd $BASEDIR
sbt compile