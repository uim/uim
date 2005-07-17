#!/bin/sh

for bench in bench/bench-*.scm
do
  echo "Running benchmark $bench..."
  time ./sscm $bench
done
