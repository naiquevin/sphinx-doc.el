#!/usr/bin/env bash

emacs -batch \
      -L . \
      -L ~/.emacs.d/elpa/s-20131223.944 \
      -l ert \
      -l ./sphinx-doc-tests.el \
      -f ert-run-tests-batch-and-exit
