#!/bin/bash
#Victor Nogueira - 1511043 & Francisco Thiesen - 1611854

FILES=*.monga
PASSED=true

for FILE in $FILES
do
    NAME=$(basename $FILE .monga)
    ../src/mongac -o $NAME.t $FILE 2> $NAME.t
    if ! diff $NAME.ans $NAME.t
    then
        echo "tokens found in $FILE differ from answer"
        PASSED=false
    fi
    rm $NAME.t
done

if $PASSED
then
    echo "TESTS PASSED!"
fi
