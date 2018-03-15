open Yojson
open Yojson.Basic.Util
open Msg_types
open IeleClientUtils

let file = Sys.argv.(1)

let () = if Array.length Sys.argv <> 3 then
  prerr_endline ("usage: " ^ Sys.argv.(0) ^ " <file.iele.json> <port>")

let json = Yojson.Basic.from_channel (open_in file)

let test =
  match json with
  `Assoc [(_, test)] -> test
| _ -> failwith "Invalid json structure: expected an object at top level."

let failed = ref false

let test_transaction header (state: (string * Basic.json) list) (tx: Basic.json) (result: Basic.json) : (string * Basic.json) list =
  let post_state, call_result = exec_transaction "gasPrice" "gasLimit" header state tx in
  let expected_return = List.map (fun json -> of_hex (json |> to_string)) (result |> member "out" |> to_list) in
  let rets = unpack_output call_result.return_data in
  let actual_return = List.map (fun arg -> `String (Bytes.to_string arg)) rets in
  if expected_return <> rets then begin
    prerr_endline ("failed " ^ file ^ ": out:\n" ^ Yojson.Basic.to_string (`List actual_return));
    failed := true;
  end;
  let expected_returncode = of_hex (result |> member "status" |> to_string) in
  if World.to_z expected_returncode <> World.to_z call_result.return_code then begin
    prerr_endline ("failed " ^ file ^ ": status:\n" ^ (to_hex call_result.return_code));
    failed := true;
  end;
  let expected_gas = of_hex (result |> member "gas" |> to_string) in
  if World.to_z_unsigned expected_gas <> World.to_z_unsigned call_result.gas_remaining then begin
    prerr_endline ("failed " ^ file ^ ": gas:\n" ^ (to_hex call_result.gas_remaining));
    failed := true;
  end;
  let expected_refund = of_hex (result |> member "refund" |> to_string) in
  if World.to_z_unsigned expected_refund <> World.to_z_unsigned call_result.gas_refund then begin
    prerr_endline ("failed " ^ file ^ ": refund:\n" ^ (to_hex call_result.gas_refund));
    failed := true;
  end;
  let expected_logs = of_hex_unsigned (result |> member "logs" |> to_string) in
  let rlp_logs = Rlp.RlpList(List.map log_to_rlp call_result.logs) in
  let actual_logs = Bytes.of_string (Cryptokit.hash_string (hash()) (Rope.to_string (Rlp.encode rlp_logs))) in
  if expected_logs <> actual_logs then begin
    prerr_endline ("failed " ^ file ^ ": logs:\nactual: " ^ (Rlp.display (rlp_to_hex rlp_logs)) ^ "\nhash: " ^ (to_hex actual_logs));
    failed := true;
  end;
  post_state

let rec canonicalize assoc =
  List.map (fun (k,v) -> match (k,v) with
  | "code", `String code -> "code",`String(if code = "" then "" else if String.sub code 0 2 = "0x" then code else to_hex (assemble code))
  | _, `Assoc a -> (k,`Assoc(sort_assoc_list (canonicalize a)))
  | _ -> k,v) assoc

let test_block state block =
  let bh = block |> member "blockHeader" in
  let beneficiary = bh |> member "coinbase" |> to_string in
  let difficulty = bh |> member "difficulty" |> to_string in
  let number = bh |> member "number" |> to_string in
  let gas_limit = bh |> member "gasLimit" |> to_string in
  let timestamp = bh |> member "timestamp" |> to_string in
  let block_header = {beneficiary=of_hex_unsigned beneficiary;difficulty=of_hex difficulty;number=of_hex number;gas_limit=of_hex gas_limit;unix_timestamp=Z.to_int64 (World.to_z (of_hex timestamp))} in
  let transactions = block |> member "transactions" |> to_list in
  let results = block |> member "results" |> to_list in
  List.fold_left2 (test_transaction block_header) state transactions results

let pre = test |> member "pre" |> to_assoc
let blocks = test |> member "blocks" |> to_list
let expected = sort_assoc_list (canonicalize (test |> member "postState" |> to_assoc))
let json_blockhashes = test |> member "blockhashes" |> to_list
let str_blockhashes = List.map to_string json_blockhashes
let blockhashes = List.map of_hex_unsigned str_blockhashes;;
List.iter World.InMemoryWorldState.add_blockhash (List.rev blockhashes);;
let actual = sort_assoc_list (canonicalize (List.fold_left test_block pre blocks));;
if expected <> actual then begin
  prerr_endline ("failed " ^ file ^ ": postState:\nexpected:" ^ Yojson.Basic.to_string (`Assoc expected) ^ "\nactual: " ^ Yojson.Basic.to_string (`Assoc actual));
  failed := true;
end;;
if !failed then failwith "assertions failed";;
