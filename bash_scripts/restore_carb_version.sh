#!/bin/bash

set -e

BASEDIR="$(dirname -- "$(dirname -- "$(realpath -- "$0")")")"

cd $BASEDIR/carbon
git checkout master
cd $BASEDIR
sbt compile
