#!/usr/bin/env bash

set -e

if [[ $(uname -s) == "Darwin" ]]; then
    EMACS_BIN=/Applications/Emacs.app/Contents/MacOS/Emacs
else
    EMACS_BIN=emacs
fi

function elpa_dep_dir() {
    echo $(find ~/.emacs.d/elpa -type d -depth 1 | grep -E "/$1-\d+\.\d+" | sort -r | head -n 1)
}

$EMACS_BIN -batch \
      -L . \
      -L $(elpa_dep_dir "dash") \
      -L $(elpa_dep_dir "s") \
      -l ert \
      -l ./sphinx-doc-tests.el \
      -f ert-run-tests-batch-and-exit
