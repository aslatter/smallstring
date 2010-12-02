#!/bin/sh

pushd test || exit 0
rm -f *.hi *.o Bench
ghc --make -O2 -package-conf ../pkg.conf.d Bench.hs -o Bench || popd; exit 0 }
./Bench
popd > /dev/null

