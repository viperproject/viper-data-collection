#!/bin/bash

set -e

BASEDIR="$(dirname -- "$(dirname -- "$(realpath -- "$0")")")"
CP_FILE="$BASEDIR/vdc_classpath.txt"

if [ ! -f "$CP_FILE" ]; then
  (
    cd "$BASEDIR"
    sbt "export runtime:dependencyClasspath" | tail -n1 >"$CP_FILE"
  )
fi

while true; do
  java -Xss128M -Xmx1g -Dlogback.configurationFile="$BASEDIR/src/main/resources/logback.xml" -cp "$(cat "$CP_FILE")" dataCollection.ProcessingPipeline
  if [ $? -eq -1 ]; then
    sleep 10
  fi
done
