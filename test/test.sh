#!/bin/bash
#Victor Nogueira - 1511043 & Francisco Thiesen - 1611854

FILES=*.monga
PASSED=true

for FILE in $FILES
do
    NAME=$(basename $FILE .monga)
    ../src/mongac -o $NAME.o $FILE 2> $NAME.o
    if ! diff $NAME.ans $NAME.o > $NAME.diff
		#&& ! diff $NAME.ans $NAME.e > $NAME.diff
    then
        echo "tokens found in $FILE differ from answer"
		cat $NAME.diff
        PASSED=false
    fi
    rm $NAME.o
    #rm $NAME.e
    rm $NAME.diff
done

if $PASSED
then
    echo "TESTS PASSED!"
fi
