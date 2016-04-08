#!/bin/bash
set -e
cabal build
./dist/build/hipam/hipam
