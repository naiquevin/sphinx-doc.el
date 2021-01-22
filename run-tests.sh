#!/usr/bin/env bash

set -e

EMACS="${EMACS:=emacs}"

NEEDED_PACKAGES="dash s"

INIT_PACKAGE_EL="(progn \
  (require 'package) \
  (push '(\"melpa\" . \"https://melpa.org/packages/\") package-archives) \
  (package-initialize) \
  (unless package-archive-contents \
     (package-refresh-contents)) \
  (dolist (pkg '(${NEEDED_PACKAGES})) \
    (unless (package-installed-p pkg) \
      (package-install pkg))))"


${EMACS} -Q -batch \
      --eval "$INIT_PACKAGE_EL" \
      -L . \
      -l ert \
      -l ./sphinx-doc-tests.el \
      -f ert-run-tests-batch-and-exit
