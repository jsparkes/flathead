module Globals

open Utility
open Type

let first_global = 16
let last_global = 255

let global_addr story (Global globall) = 
  if globall < first_global || globall > last_global then
      failwith "global variable index out of range"
  else
    let (Global_table_base baase) = Story.global_variables_table_base story in
    let baase = Word_address baase in
    let offset = globall - first_global in
    inc_word_addr_by baase offset

let read story globall =
    Story.read_word story (global_addr story globall)

let write story globall value =
    Story.write_word story (global_addr story globall) value
    
let display story =
  let to_string g =
    Printf.sprintf "%02x %04x\n" (g - first_global) (read story (Global g)) in
  accumulate_strings_loop to_string first_global (last_global + 1)

