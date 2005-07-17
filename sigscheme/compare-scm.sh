#!/bin/sh

echo "Running benchmark $bench..."
echo "[ SigScheme ]"
time ./sscm $1
echo "[ SIOD ]"
time uim-sh -B < $1
echo "[ Gauche ]"
time gosh $1
