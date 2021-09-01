#!/usr/bin/env bash
set -euo pipefail

clj -X:depstar

native-image --report-unsupported-elements-at-runtime \
             --initialize-at-build-time \
             --no-server \
             -jar app.jar
