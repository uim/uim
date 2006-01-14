#!/bin/sh

SSCM=src/sscm
TEST=test/test-tail-rec.scm

ulimit -s 128 && ulimit -d 2048 && $SSCM $TEST || echo 'correctly exploded'
