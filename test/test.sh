#!/bin/bash
#Victor Nogueira - 1511043 & Francisco Thiesen - 1611854

FILES=*.monga
PASSED=true

for FILE in $FILES
do
    NAME=$(basename $FILE .monga)
    ../src/mongac -o $NAME.o $FILE 2> $NAME.o
    if ! diff $NAME.ans $NAME.o
    then
        echo "tokens found in $FILE differ from answer"
        PASSED=false
    fi
    rm $NAME.o
done

if $PASSED
then
    echo "TESTS PASSED!"
fi
