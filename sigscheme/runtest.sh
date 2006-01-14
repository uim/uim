#!/bin/sh

SSCM=src/sscm


echo "[ Run Test ported from Bigloo]"
for test in test/bigloo-*.scm
do
  echo "Running test $test..."
  $SSCM $test
  echo
done

echo "[ Run Test ported from Gauche ]"
for test in test/gauche-*.scm
do
  echo "Running test $test..."
  $SSCM $test
  echo
done

echo "[ Run SigScheme Test ]"
for test in `ls test/test-*.scm | grep -v test-tail-rec\.scm`
do
  echo "Running test $test..."
  $SSCM $test
  echo
done
