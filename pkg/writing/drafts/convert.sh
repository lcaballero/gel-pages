#!/bin/bash
set -e

DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" >/dev/null 2>&1 && pwd)"

clean() {
  (cd "$DIR" && rm -rf *.gen.html *.out)
}

session() {
    script -q "$DIR/snippet.$1.out" ./convert.sh show "$1"
}

show() {
  bat --paging=never --unbuffered -l perl "code-$1.txt"
}

capture() {
  local n
  n="$1"
  cat "snippet.$1.out" | \
    aha --black --no-header | \
    tee "code-$1.gen.html"
}

main() {
  clean
  seq 1 3 | xargs -I{} ./convert.sh session {}
  seq 1 3 | xargs -I{} ./convert.sh capture {}
}

"$@"
