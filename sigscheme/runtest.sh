#!/bin/sh

for test in test/test-*.scm
do
  echo "Running test $test..."
  ./sscm $test
done
