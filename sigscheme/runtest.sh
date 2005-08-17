#!/bin/sh


echo "[ Run Test ported from Bigloo]"
for test in test/bigloo-*.scm
do
  echo "Running test $test..."
  ./sscm $test
done

echo "[ Run Test ported from Gauche ]"
for test in test/gauche-*.scm
do
  echo "Running test $test..."
  ./sscm $test
done

echo "[ Run SigScheme Test ]"
for test in test/test-*.scm
do
  echo "Running test $test..."
  ./sscm $test
done

