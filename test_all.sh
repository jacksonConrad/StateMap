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
	echo "Compiling ... " >> "$LOGFILE"
	("$COMPILER" < "$TESTFILE") 2>> "$LOGFILE"

	# if compilation succeeds, run output.py.
	if (find output.py &>/dev/null)
	then
		echo "Python runtime output:" >> "$LOGFILE"
		(python "$COMPFILE" > "$OUTFILENAME") 2>> "$LOGFILE"
		echo "Diff:\n" >> "$LOGFILE"
		touch "$OUTFILENAME"
		if (colordiff "$OUTFILENAME" "$TESTFILENAME" >/dev/null) 
		then
			colordiff "$OUTFILENAME" "$TESTFILENAME" 
			echo 'OK!' | tee -a "$LOGFILE"
		else
			colordiff "$OUTFILENAME" "$TESTFILENAME" 
			echo "BAD!" | tee -a "$LOGFILE"
		fi
	else
		echo "BAD!\nCompilation of $TESTFILE FAILED" | tee -a "$LOGFILE"
	fi
	touch output.py
	rm '$COMPFILE' "$OUTFILENAME" &>/dev/null
done