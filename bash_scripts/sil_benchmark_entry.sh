#!/bin/bash

set -e

BASEDIR="$(dirname --  "$(dirname -- "$(realpath -- "$0")")")"
CP_FILE="$BASEDIR/vdc_classpath.txt"

if [ ! -f "$CP_FILE" ]; then
    (cd "$BASEDIR"; sbt "export runtime:dependencyClasspath" | tail -n1 > "$CP_FILE")
fi

exec java -Xss30M -Dlogback.configurationFile="$BASEDIR/silicon/src/main/resources/logback.xml" -cp "$(cat "$CP_FILE")" dataCollection.SiliconBenchmarkRunner "$@"
