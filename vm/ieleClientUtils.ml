open Yojson
open Yojson.Basic.Util
open Msg_types

let hash () = Cryptokit.Hash.keccak 256

let sort_assoc_list l =
  List.sort (fun (a,_) (b,_) -> compare a b) l

(** of_hex str takes a hex-encoded string and converts it to a sequence of bytes.
    of_hex ~signed:true also interprets the string as a signed integer *)
let of_hex ?signed:(signed=false) str =
  if str = "" then Bytes.empty else
  let str,neg = if String.sub str 0 1 = "-" then String.sub str 1 (String.length str - 1),true else str,false in
  let str = if String.sub str 0 2 = "0x" then String.sub str 2 (String.length str - 2) else str in
  let str = Hex.to_string (`Hex str) in
  let str = if signed && not neg && String.length str > 0 && World.is_negative str.[0] then "\000" ^ str else str in
  let res = Bytes.of_string str in
  if neg then World.of_z (Z.neg (World.to_z res)) else if signed then World.of_z (World.to_z res) else res

let to_hex bytes =
  if Bytes.length bytes = 0 then "0x00" else
  let str = Bytes.to_string bytes in
  let str = if String.length str > 1 && str.[0] = '\000' then
    String.sub str 1 (String.length str - 1)
  else
    str
  in
  "0x" ^ (match Hex.of_string str with (`Hex str) -> str)

let to_hex_unsigned bytes =
  let str = Bytes.to_string bytes in
  "0x" ^ (match Hex.of_string str with (`Hex str) -> str)

let abs_path rel =
  if Filename.is_relative rel then (Sys.getcwd ()) ^ "/" ^ rel else rel

let assemble file =
  let file_dir = Filename.dirname Sys.argv.(1) in
  let abs_file_dir = abs_path file_dir in
  let _in = Unix.open_process_in("iele-assemble " ^ (Filename.quote (abs_file_dir ^ "/" ^ file))) in
  let result = input_line _in in
  match Unix.close_process_in _in with
  | Unix.WEXITED 0 -> of_hex result
  | _ -> prerr_endline result; failwith ("failed to assemble " ^ file)

let get_code code =
  if code = "" then Bytes.empty else if String.sub code 0 2 = "0x" then of_hex code else assemble code

let checkpoint pre gas_provided gas_price origin : (string * Basic.json) list =
  List.map (fun (k,v) ->
    if of_hex k = origin then match v with
    | `Assoc a ->
      let without = List.remove_assoc "nonce" (List.remove_assoc "balance" a) in
      let new_nonce = World.of_z (Z.add Z.one (World.to_z_unsigned (of_hex ~signed:true (v |> member "nonce" |> to_string)))) in
      let gas_payment = Z.mul (World.to_z_unsigned gas_price) (World.to_z_unsigned gas_provided) in
      let new_balance = World.of_z (Z.sub (World.to_z_unsigned (of_hex ~signed:true (v |> member "balance" |> to_string))) gas_payment) in
      (k,`Assoc(("nonce", `String(to_hex new_nonce))::("balance", `String(to_hex new_balance))::without))
    | _ -> failwith "Invalid value where json object expected"
    else (k,v)) pre

let add_account (id,data) =
  let acctID = of_hex id in
  let old_nonce = data |> member "nonce" |> to_string in
  let old_balance = data |> member "balance" |> to_string in
  let code = data |> member "code" |> to_string in
  let asm_code = get_code code in
  let storage = data |> member "storage" |> to_assoc in
  let nonce,balance = of_hex ~signed:true old_nonce,of_hex ~signed:true old_balance in
  let map = List.fold_left (fun map (k,v) -> World.StringMap.add (Bytes.to_string (of_hex ~signed:true k)) (of_hex ~signed:true (v |> to_string)) map) World.StringMap.empty storage in
  World.InMemoryWorldState.add_account acctID nonce balance asm_code map

let init_state state =
  World.InMemoryWorldState.reset_state ();
  List.iter add_account state

let pack_input args str =
  let l = List.map (fun arg -> Rlp.RlpData (Rope.of_string (Bytes.to_string arg))) args in
  let rlp = Rlp.RlpList[Rlp.RlpData (Rope.of_string str);Rlp.RlpList l] in
  Bytes.of_string (Rope.to_string (Rlp.encode rlp))

let unpack_output data =
  let rlp = Rlp.decode (Rope.of_string (Bytes.to_string data)) in
  match rlp with
  Rlp.RlpList(rets) ->
  List.map (fun rlp -> World.of_z (VM.z_of_rlp rlp)) rets
| _ -> failwith "Invalid value where rlp-encoded return values expected"

let rlp_of_bytes (b: bytes) : Rlp.t =
  Rlp.RlpData (Rope.of_string (Bytes.to_string b))

let log_to_rlp (log: log_entry) : Rlp.t =
  Rlp.RlpList[rlp_of_bytes log.address; Rlp.RlpList(List.map rlp_of_bytes log.topics); rlp_of_bytes log.data]

module StringSet = Set.Make(String)

let update_storage_entry (storage: (string * Basic.json) list) (update: storage_update) : (string * Basic.json) list =
  let key = to_hex update.offset in
  let z_value = World.to_z update.data in
  let value = to_hex update.data in
  let without = List.remove_assoc key storage in
  if Z.equal z_value Z.zero then without else
  (key,`String value) :: without

let update_storage (storage: (string * Basic.json) list) (updates: storage_update list) : (string * Basic.json) list =
  sort_assoc_list (List.fold_left update_storage_entry storage updates)

let mod_acct_to_json (pre: (string * Basic.json) list) (acct: modified_account) : string * Basic.json =
  let address = to_hex_unsigned acct.address in
  let storage,old_code = try
    let account = List.assoc address pre in
    let storage = account |> member "storage" |> to_assoc in
    let code = account |> member "code" |> to_string in
    storage,code
  with Not_found -> [],"" in
  let new_code = if Bytes.length acct.code = 0 then
    old_code
  else
    to_hex_unsigned acct.code
  in
  (address,`Assoc(
    [("nonce",`String(to_hex acct.nonce));
     ("balance",`String(to_hex acct.balance));
     ("code",`String(new_code));
     ("storage",`Assoc(update_storage storage acct.storage_updates))]))

let update_state (pre: (string * Basic.json) list) (mod_accts: modified_account list) del_accts =
  let del_accts_set = StringSet.of_list (List.map to_hex_unsigned del_accts) in
  let mod_accts_set = StringSet.of_list (List.map (fun (acct: modified_account) -> to_hex_unsigned acct.address) mod_accts) in
  let deleted = List.filter (fun (acct,data) -> not (StringSet.mem acct del_accts_set || StringSet.mem acct mod_accts_set)) pre in
  deleted @ (List.map (mod_acct_to_json pre) mod_accts)

let rec rlp_to_hex = function
  | Rlp.RlpData str -> Rlp.RlpData (Rope.of_string (to_hex (Bytes.of_string (Rope.to_string str))))
  | Rlp.RlpList l -> Rlp.RlpList (List.map rlp_to_hex l)

let send_request ctx =
  let addr = Unix.ADDR_INET(Unix.inet_addr_loopback,(int_of_string Sys.argv.(2))) in
  World.send addr ctx

let exec_transaction signed gasPrice gasLimit header (state: (string * Basic.json) list) (tx: Basic.json) : (string * Basic.json) list * call_result =
  let gas_price = of_hex ~signed:signed (tx |> member gasPrice |> to_string) in
  let gas_provided = of_hex ~signed:signed (tx |> member gasLimit |> to_string) in
  let owner = tx |> member "to" |> to_string in
  let txcreate = owner = "" || owner = "0x" in
  let from = tx |> member "from" |> to_string in
  let origin = of_hex from in
  let checkpoint_state = checkpoint state gas_price gas_provided origin in
  init_state checkpoint_state;
  let args = List.map (fun json -> of_hex ~signed:signed (json |> to_string)) (tx |> member "arguments" |> to_list) in
  let value = tx |> member "value" |> to_string in
  let data = if txcreate then
    let data_str = tx |> member "contractCode" |> to_string in
    Bytes.to_string (get_code data_str)
  else
    tx |> member "function" |> to_string
  in
  let txdata = pack_input args data in
  let g0 = VM.g0 txdata txcreate in
  let gas_provided = Z.sub (World.to_z_unsigned gas_provided) g0 in
  let ctx = {recipient_addr=of_hex owner;caller_addr=origin;input_data=txdata;call_value=of_hex ~signed:signed value;gas_price=gas_price;gas_provided=World.of_z gas_provided;block_header=Some header;config=Iele_config} in
  let call_result = send_request ctx in
  let post_state = update_state checkpoint_state call_result.modified_accounts call_result.deleted_accounts in
  post_state, call_result
