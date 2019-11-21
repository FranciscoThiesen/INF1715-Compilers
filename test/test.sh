FILES=*.monga
PASSED=true

for FILE in $FILES
do
    NAME=$(basename $FILE .monga)
	../src/mongac -o $NAME.ll $FILE
	llc-mp-5.0 -o $NAME.s $NAME.ll
	gcc -Wall $NAME.s -fno-pie
	./a.out > $NAME.o
    if ! diff $NAME.ans $NAME.o
    then
        echo "output found in $FILE differ from answer"
        PASSED=false
    fi
    rm $NAME.s
    rm $NAME.ll
	rm $NAME.o
	rm a.out
done

if $PASSED
then
    echo "TESTS PASSED!"
fi
