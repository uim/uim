#!/bin/sh

SSCM=src/sscm

for bench in bench/bench-*.scm
do
  echo "Running benchmark $bench..."
  time $SSCM $bench
done
