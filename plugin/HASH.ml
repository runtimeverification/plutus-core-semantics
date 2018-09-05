open Constants
open Constants.K

let do_hash_str (str: string) (h: Cryptokit.hash) : k =
  let bytes = Cryptokit.hash_string h str in
  let buf = Buffer.create ((String.length bytes) * 2) in
  String.iter (fun c -> Buffer.add_string buf (Printf.sprintf "%02x" (int_of_char c))) bytes;
  [String (Buffer.contents buf)]

let do_hash_bytes (bytes: bytes) (h: Cryptokit.hash): k =
  [Bytes (Bytes.of_string (Cryptokit.hash_string h (Bytes.to_string bytes)))]

let do_hook
  (h: Cryptokit.hash) (failMsg: string)
  (c: k) (lbl: klabel) (sort: sort) (config: k) (ff: string -> k -> Z.t -> k): k =
  match c with [String str]  ->  do_hash_str str h
             | [Bytes bytes] ->  do_hash_bytes bytes h
             | _ -> failwith failMsg

let hook_sha2_256  = do_hook (Cryptokit.Hash.sha2 256)      "sha2_256"
let hook_sha3_256  = do_hook (Cryptokit.Hash.sha3 256)      "sha3_256"
let hook_keccak256 = do_hook (Cryptokit.Hash.keccak 256)    "keccak256"
let hook_ripemd160 = do_hook (Cryptokit.Hash.ripemd160 ())  "ripemd160"
