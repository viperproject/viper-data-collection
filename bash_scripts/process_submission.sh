#!/bin/bash

CURDIR="$PWD"
BASEDIR="$(dirname -- "$(dirname -- "$(realpath -- "$0")")")"
CP_FILE="$BASEDIR/vdc_classpath.txt"

if [ ! -f "$CP_FILE" ]; then
  (
    cd "$BASEDIR"
    sbt "export runtime:dependencyClasspath" | tail -n1 >"$CP_FILE"
    cd "$CURDIR"
  )
fi

while true; do
  java -Xss128M -Xmx1g -Dlogback.configurationFile="$BASEDIR/src/main/resources/logback.xml" -cp "$(cat "$CP_FILE")" dataCollection.ProcessingPipeline
  if [ $? -ne 0 ]; then
    sleep 10
  fi
done
