#!/bin/bash
set -e

DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" >/dev/null 2>&1 && pwd)"

# build create
build() {
  rm -rf "$DIR/.dist" \
     && mkdir -p "$DIR/.dist" \
     && (cd "$DIR" && cp -r pkg/www/html/assets/{css,fonts,img,js} "$DIR/.dist") \
     && (cd "$DIR/pkg/src" && go generate ./... && go install)
}

dist() {
  build && gel-pages write \
                     --root .dist \
                     --posts posts \
                     --base "https://www.read-later.net"
}

tests() {
  (cd "$DIR/pkg/src" && go test ./...)
}

# local-module is a snippet of code ran to bring a local version of
# the gel module
local-module() {
  go mod edit --replace=github.com/lcaballero/gel-pages=../gel
}

gen::files() {
  find "$DIR" -type f -name '*.gen.go'
}

gen::clean() {
  gen::files | xargs rm
}

"$@"
