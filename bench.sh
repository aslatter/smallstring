#!/usr/bin/env bash

pushd test || exit 0
rm -f *.hi *.o Bench
ghc -Wall --make -O2 -package-conf ../pkg.conf.d Bench.hs -o Bench || popd; exit 0 }
./Bench
popd > /dev/null

