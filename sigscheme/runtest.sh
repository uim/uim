#!/bin/sh

SSCM=src/sscm

if test "x$1" != "x"; then
  while test "$#" -ne 0; do
    $SSCM "$1"
    shift
  done
  exit 0
fi

run_test () {
    echo "Running test $1..."
    $SSCM $1
    echo
}

echo "[ Run single ported tests ]"
for test in test/test-r4rs.scm #test/r5rs_pitfall_sscm.scm
do
  run_test $test
done

echo "[ Run tests ported from Bigloo]"
for test in test/bigloo-*.scm
do
  run_test $test
done

echo "[ Run tests ported from Gauche ]"
for test in test/gauche-*.scm
do
  run_test $test
done

echo "[ Run SigScheme tests ]"
for test in `ls test/test-*.scm | egrep -v 'test-(tail-rec|r4rs)\.scm'`
do
  run_test $test
done

echo "Run also runtest-tail-rec.sh for proper tail recursions."
