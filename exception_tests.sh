#!/bin/bash
#script used for reg testing
COMPILER="./compiler"
COMPFILE='output.py'
LOGFILE='log_fail.txt'

rm -f "$LOGFILE" &>/dev/null
for TESTFILE in ./tests/to_fail/*.sm;
do
 	echo "	TESTING $TESTFILE" | tee -a "$LOGFILE"
	LEN=$((${#TESTFILE}-3))
	OUTFILENAME="${TESTFILE:0:$LEN}.output"
	TESTFILENAME="${TESTFILE:0:$LEN}.out"
	echo "Compiling ... " >> "$LOGFILE"
	("$COMPILER" < "$TESTFILE") 2> "$OUTFILENAME"
	if (egrep -f  "$TESTFILENAME" "$OUTFILENAME" >> "$LOGFILE" 2>&1)
	then
		echo "OK!"
	else
		echo "BAD!"
		colordiff -y "$OUTFILENAME" "$TESTFILENAME" 2>> "$LOGFILE"
	fi
	rm '$COMPFILE' "$OUTFILENAME" &>/dev/null
done
exit 0
