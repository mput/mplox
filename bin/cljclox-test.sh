#!/usr/bin/env bash
set -euo pipefail

clj -M:test $@
