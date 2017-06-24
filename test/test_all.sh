COUNT=1
NUM_ERRORS=0

function report_case () {
  if [ $1 == 0 ]
  then printf "\e[94m%-2s\e[0m %-1s Running \e[1m%-30s\e[0m \e[32m%-30s\e[0m\n" "$COUNT" "|" $2 "SUCCESS"
  else
    printf "\e[94m%-2s\e[0m %-1s Running \e[1m%-30s\e[0m \e[31m%-30s\e[0m\n" "$COUNT" "|" $2 "FAILURE";
    (( NUM_ERRORS += 1 ));
  fi
}

function final_report () {
  if [ $NUM_ERRORS == 0 ]
  then (printf "    \e[1m\e[32m$(( COUNT-1 )) programs have been successfully run.\e[0m\n"; exit 0)
  else (printf "    \e[1m\e[31m$NUM_ERRORS out of $(( COUNT-1 )) programs did not match the expected output."; exit 1)
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
printf -- "---------------------------------------------------\n"

final_report
