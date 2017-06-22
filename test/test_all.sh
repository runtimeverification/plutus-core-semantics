for file in `ls *.plcore`;
  do
    krun -d .. $file | diff $file.out -;
    if [ $? != 0 ]
    then
      printf "Running \x1B[1m%-30s\x1B[0m  \x1B[31m%-30s\e[0m\n" $file "FAILURE"
    else
      printf "Running \x1B[1m%-30s\x1B[0m \x1B[32m%-30s\e[0m\n" $file "SUCCESS"
    fi
done
