#!/bin/sh

TEST=test/test-tail-rec.scm

ulimit -s 128 && ulimit -d 2048 && ./sscm $TEST || echo 'correctly exploded'
