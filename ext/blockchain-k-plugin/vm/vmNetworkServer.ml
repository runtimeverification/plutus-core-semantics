Sys.catch_break true;;

let usage = "Usage: vm PORT BIND_ADDRESS [debug]"
let arg_error msg = prerr_string msg; prerr_newline (); prerr_endline usage; exit 1;;

if Array.length Sys.argv == 2 && Sys.argv.(1) == "--help" then
  (print_endline usage; exit 0)
else if Array.length Sys.argv < 3 then
  arg_error "Not enough arguments"
else if Array.length Sys.argv > 4 then
  arg_error "Too many arguments"
else if Array.length Sys.argv == 4 && Sys.argv.(3) <> "debug" then
  arg_error ("Invalid third argument "^Sys.argv.(3));;

let port = try int_of_string Sys.argv.(1)
  with Failure _ -> arg_error ("Invalid port number "^Sys.argv.(1))
in let host = try Unix.inet_addr_of_string Sys.argv.(2)
  with Failure _ -> arg_error ("Invalid address "^Sys.argv.(2))
in let addr = Unix.ADDR_INET(host,port) in
World.serve addr VM.run_transaction
