let addr = Unix.ADDR_INET((Unix.inet_addr_of_string Sys.argv.(2)),(int_of_string Sys.argv.(1))) in
World.serve addr VM.run_transaction
