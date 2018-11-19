#!/usr/bin/env bash

set -e

LOADER_MODULE=./output/Loader/index.js
LOADER_BINDING=loader
OUTPUT=./index.js

WEBPACK_ENTRY=$(mktemp -p .)
clean_up() {
    ARG=$?
    rm "$WEBPACK_ENTRY"
    exit $ARG
}
trap clean_up EXIT

cat <<EOF >"$WEBPACK_ENTRY"
module.exports=require('${LOADER_MODULE}').${LOADER_BINDING};
EOF

psc-package build

./node_modules/.bin/webpack \
    --mode production \
    --target node \
    --entry "$WEBPACK_ENTRY" \
    -o "$OUTPUT" \
    --output-library-target commonjs-module
