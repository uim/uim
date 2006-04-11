#!/bin/sh

SSCM=src/sscm

if test "x$1" != "x"; then
  while test "$#" -ne 0; do
    $SSCM "$1"
    shift
  done
  exit 0
fi

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
