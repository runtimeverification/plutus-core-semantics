COUNT=1
function report () {
  if [ $1 == 0 ]
  then
    printf "\x1B[94m%-6s\x1B[0m Running \x1B[1m%-30s\x1B[0m \x1B[32m%-30s\e[0m\n" "[$COUNT]" $2 "SUCCESS"
  else
    printf "\x1B[94m%-6s\x1B[0m Running \x1B[1m%-30s\x1B[0m \x1B[31m%-30s\e[0m\n" "[$COUNT]" $2 "FAILURE"
  fi
}

for file in `ls *.plcore`;
  do
    krun -d .. $file | diff $file.out - > /dev/null;
    if [ $? != 0 ]
      then report 1 $file
      else report 0 $file
    fi;
    (( COUNT+=1 ));
done
