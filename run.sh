#!/bin/bash
set -e

# build create
build() {
  rm -rf "$(pwd)/.dist" \
     && mkdir -p .dist \
     && cp -r pkg/www/html/assets/{css,fonts,img,js} .dist \
     && (cd pkg/src && go generate ./...) \
     && (cd pkg/src && go install)
}

dist() {
  build && \
    gel-pages write \
              --root .dist \
              --posts posts \
              --home before-i-get-started
}

list() {
  gel-pages list
}

# local-module is a snippet of code ran to bring a local version of
# the gel module
local-module() {
  go mod edit --replace=github.com/lcaballero/gel-pages=../gel
}

gen::files() {
  find . -type f -name '*.gen.go'
}

gen::clean() {
  gen::files | xargs rm
}


"$@"

