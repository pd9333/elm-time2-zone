#!/usr/bin/env just --justfile

build:
  sh build.sh

test:
  elm-verify-examples
  elm-test

doc:
  elm-doc-preview

# Local Variables:
# mode: makefile
# End:
# vim: set ft=make :
