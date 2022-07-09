#!/bin/bash
set -e

DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" >/dev/null 2>&1 && pwd)"

build() {
  (cd "$DIR/pkg/src" && go generate ./... && go install)
}
