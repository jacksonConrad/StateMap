#!/bin/bash
#script used for reg testing
COMPILER="../compiler"
COMPFILE='output.py'

for TESTFILE in ../tests/*.sm;
do
 	echo "	TESTING $TESTFILE"
	LEN=$((${#TESTFILE}-3))
	OUTFILENAME="${TESTFILE:0:$LEN}.output"
	TESTFILENAME="${TESTFILE:0:$LEN}.out"
	"$COMPILER" < "$TESTFILE"
	python "$COMPFILE" > "$OUTFILENAME"
	if (colordiff "$OUTFILENAME" "$TESTFILENAME") 
	then
		echo "		OK"
	else
		echo "		BAD!"
	fi
	rm "$OUTFILENAME" "$COMPFILE"
done