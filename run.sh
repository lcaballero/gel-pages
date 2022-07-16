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
                     --root "$DIR/.dist" \
                     --posts posts \
                     --base "https://www.read-later.net"
}

prod() {
  build && gel-pages write \
                     --root "$DIR/.dist" \
                     --posts posts \
                     --base "https://www.read-later.net" \
                     --env prod
}

tests() {
  (cd "$DIR/pkg/src" && go test ./...)
}

clean() {
  rm -rf "$DIR/.dist"
}

# local-module is a snippet of code ran to bring a local version of
# the gel module
local-module() {
  go mod edit --replace=github.com/lcaballero/gel-pages=../gel
}

lint() {
  (cd "$DIR/pkg/src" && golangci-lint run -v)
}

gen::files() {
  find "$DIR" -type f -name '*.gen.go'
}

gen::clean() {
  gen::files | xargs rm
}

img::dockerfile() {
    cat <<EOF
FROM ubuntu:bionic

RUN apt update
ENV DEBIAN_FRONTEND=noninteractive
RUN apt install -y imagemagick
RUN apt-get install -y webp jpegoptim optipng

EOF

}

img::sh() {
  (cd dl/images && \
     docker run -it --rm \
            -v $(pwd):/app \
            -w /app \
            mogrify:wip \
            bash
   )
}

img::run() {
  rm -rf dl/out && mkdir -p dl/out && \
    docker run -it --rm \
           -v $(pwd)/dl:/app \
           -w /app \
           mogrify:wip \
           jpegoptim -v \
           --dest=./out \
           --strip-all \
           --size=30% \
           --max=100 \
           images/julien-riedel-kHklwdauyHI-unsplash.jpg

  cp dl/out/julien-riedel-kHklwdauyHI-unsplash.jpg pkg/www/html/assets/img/
}

img() {
  (cd bin && \
     img::dockerfile | docker build -t mogrify:wip . -f -)
}

cp::bm() {
  cp '/Users/lucascaballero/Library/Application Support/Google/Chrome/Default/Bookmarks' \
     bookmarks.json
}

"$@"
