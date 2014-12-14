#!/bin/bash
#script used for reg testing
COMPILER="./compiler"
COMPFILE='output.py'
LOGFILE='log.txt'

rm -f "$LOGFILE" &>/dev/null
for TESTFILE in ./tests/*.sm;
do
 	echo "	TESTING $TESTFILE" | tee -a "$LOGFILE"
	LEN=$((${#TESTFILE}-3))
	OUTFILENAME="${TESTFILE:0:$LEN}.output"
	TESTFILENAME="${TESTFILE:0:$LEN}.out"
	echo "\t\tCompiling ... " >> "$LOGFILE"
	("$COMPILER" < "$TESTFILE") 2>> "$LOGFILE"

	# if compilation succeeds, run output.py.
	if (find output.py &>/dev/null)
	then
		echo "\t\tPython runtime output:" >> "$LOGFILE"
		(python "$COMPFILE" > "$OUTFILENAME") 2>> "$LOGFILE"
		echo "\t\tDiff:\n" >> "$LOGFILE"
		touch "$OUTFILENAME"
		if (diff "$OUTFILENAME" "$TESTFILENAME" | tee -a "$LOGFILE") 
		then
			echo "\t\tOK!" | tee -a "$LOGFILE"
		else
			echo "\t\tBAD!" | tee -a "$LOGFILE"
		fi
	else
		echo "\t\tBAD!\n\t\tCompilation of $TESTFILE FAILED" | tee -a "$LOGFILE"
	fi
	touch output.py
	rm "$OUTFILENAME" "$COMPFILE" &>/dev/null
done