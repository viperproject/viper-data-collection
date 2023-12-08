#!/bin/bash

set -e

BASEDIR="$(dirname -- "$(dirname -- "$(realpath -- "$0")")")"

cd $BASEDIR/carbon
git rev-parse --short HEAD
