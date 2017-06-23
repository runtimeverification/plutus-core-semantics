COUNT=1
NUM_ERRORS=0

function report_case () {
  if [ $1 == 0 ]
  then printf "\x1B[94m%-2s\x1B[0m %-1s Running \x1B[1m%-30s\x1B[0m \x1B[32m%-30s\e[0m\n" "$COUNT" "|" $2 "SUCCESS"
  else
    printf "\x1B[94m%-2s\x1B[0m %-1s Running \x1B[1m%-30s\x1B[0m \x1B[31m%-30s\e[0m\n" "$COUNT" "|" $2 "FAILURE";
    (( NUM_ERRORS += 1 ));
  fi
}

function final_report () {
  if [ $NUM_ERRORS == 0 ]
  then (echo "    \x1B[1m\x1B[32m$(( COUNT-1 )) programs have been successfully run.\x1B[0m"; exit 0)
  else (echo "    \x1B[1m\x1B[31m$NUM_ERRORS out of $(( COUNT-1 )) programs did not match the expected output."; exit 1)
  fi
}

for file in `ls *.plcore`;
  do
    krun -d .. $file | diff $file.out - > /dev/null;
    if [ $? != 0 ]
      then report_case 1 $file
      else report_case 0 $file
    fi;
    (( COUNT+=1 ));
done;
printf "\n---------------------------------------------------\n"

final_report
