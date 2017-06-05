for file in `ls test/*.plcore`;
  do
    echo "Running $file..."
    echo "------------------------------------------------------------------------"
    kast $file
    echo ""
done
