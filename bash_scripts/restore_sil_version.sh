#!/bin/bash

set -e

BASEDIR="$(dirname --  "$(dirname -- "$(realpath -- "$0")")")"

cd BASEDIR/silicon
git checkout master
cd BASEDIR
sbt compile