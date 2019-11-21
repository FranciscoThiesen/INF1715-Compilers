#!/bin/bash
#Victor Nogueira - 1511043 & Francisco Thiesen - 1611854

FILES=*.monga
PASSED=true

for FILE in $FILES
do
    NAME=$(basename $FILE .monga)
  ../src/mongac -o $NAME.ll $FILE
  llc-mp-5.0 -o $NAME.s $NAME.ll
  gcc -Wall $NAME.s -fno-pie
  ./a.out > $NAME.ans
done

if $PASSED
then
    echo "TESTS PASSED!"
fi
