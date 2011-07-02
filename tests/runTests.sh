#!/bin/sh
cabal configure
cabal build

rm -f configurator-test.tix
./dist/build/configurator-test/configurator-test

HPCDIR=dist/hpc

rm -rf $HPCDIR
mkdir -p $HPCDIR

EXCLUDES='--exclude=Main
          --exclude=Data.Configurator.Types'
hpc markup $EXCLUDES --destdir=$HPCDIR configurator-test

rm -f configurator-test.tix
