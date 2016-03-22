module Immutable_bytes 

open Type
open Utility

type t =
    {
      original_bytes : byte[];
      edits : Map<int, byte>
    }

let make bytes =
  { original_bytes = bytes; edits = Map.empty }

let size bytes =
  Array.length bytes.original_bytes

let read_byte bytes address =
  if is_out_of_range address (size bytes) then
    failwith "address is out of range"
  else
    let (Byte_address addr) = address in
    match Map.tryFind addr bytes.edits with
      | Some b -> int_of_byte b
      | None -> int_of_byte (bytes.original_bytes.[addr])

let write_byte bytes address value =
  if is_out_of_range address (size bytes) then
    failwith "address is out of range"
  else
    let (Byte_address addr) = address in
    let b = byte value in
    { bytes with edits = Map.add addr b bytes.edits }

let original bytes =
  { bytes with edits = Map.empty }
