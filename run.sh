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
  mkdir -p .dist/posts/side-projects \
        .dist/posts/possible-site-org \
        .dist/posts/cli-snippets \
        .dist/posts/site-design-tools \
        .dist/posts/organizing-pins \
        .dist/posts/first-steps-of-building-this-site

  gel-pages before-i-get-started > .dist/index.html
  gel-pages side-projects > .dist/posts/side-projects/index.html
  gel-pages possible-site-org > .dist/posts/possible-site-org/index.html
  gel-pages cli-snippets > .dist/posts/cli-snippets/index.html
  gel-pages site-design-tools > .dist/posts/site-design-tools/index.html
  gel-pages organizing-pins > .dist/posts/organizing-pins/index.html
  gel-pages first-steps-of-building-this-site > .dist/posts/first-steps-of-building-this-site/index.html
}

create() {
  build && dist
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
