module Quetzal

open Iff
open Type
open Utility

let ifzs_ifhd =
  Record [
    Header (string_to_bytes "IFhd");
    Length None;
    Integer16 None;       (* release number *)
    ByteString (None, 6); (* serial number *)
    Integer16 None;       (* checksum *)
    Integer24 None]       (* program counter *)

let ifzs_frame =
  Record [
    Integer24 None; (* return address *)
    BitField [
      Assign ("v", Integer4 None);  (* count of local variables *)
      Bit (4, None)]; (* caller discards result *)
    Integer8 None;    (* variable caller will store result in *)
    BitField [
      Bit (0, None);  (* argument 0 was supplied *)
      Bit (1, None);  (* argument 1 was supplied *)
      Bit (2, None);  (* argument 2 was supplied *)
      Bit (3, None);  (* argument 3 was supplied *)
      Bit (4, None);  (* argument 4 was supplied *)
      Bit (5, None);  (* argument 5 was supplied *)
      Bit (6, None)]; (* argument 6 was supplied *)
    Assign ("n", Integer16 None); (* size of evaluation stack in words *)
    SizedList (Lookup "v", [Integer16 None]); (* local variables *)
    SizedList (Lookup "n", [Integer16 None])] (* evaluation stack *)

let ifzs_stacks =
  Record [
    Header (string_to_bytes "Stks");
    Length None;
    UnsizedList [ifzs_frame]]

let ifzs_cmem =
  Record [
    Header (string_to_bytes "CMem");
    Length None;
    RemainingBytes None]

let ifzs_umem =
  Record [
    Header (string_to_bytes "UMem");
    Length None;
    RemainingBytes None]

let ifzd_form =
  Record [
    Header (string_to_bytes "FORM");
    Length None;
    SubHeader (string_to_bytes "IFZS");
    UnorderedList [
      ifzs_ifhd;
      ifzs_stacks;
      ifzs_umem;
      ifzs_cmem]]

let save
  (Release_number release)
  (Serial_number serial)
  (Checksum checksum)
  (Instruction pc)
  (Compressed compressed) frames =
  Record [
    Header (string_to_bytes "FORM");
    Length None; (* The writer will figure it out *)
    SubHeader (string_to_bytes "IFZS");
    UnorderedList [
      Record [
        Header (string_to_bytes "IFhd");
        Length None;
        Integer16 (Some release);
        ByteString (Some (string_to_bytes serial), 6);
        Integer16 (Some checksum);
        Integer24 (Some pc) ];
      Record [
        Header (string_to_bytes "CMem");
        Length None;
        RemainingBytes (Some compressed)];
      Record [
        Header (string_to_bytes "Stks");
        Length None;
        UnsizedList frames] ] ]

let read_ifzd_chunks ifzd =
  let form_bytes = (string_to_bytes "FORM")
  let izfs_bytes = (string_to_bytes "IZFS")
  match ifzd with
  | Record [
              Header form_bytes;
              Length _;
              SubHeader (izfs_bytes);
              UnorderedList items] ->
      items
  | _ -> failwith "TODO: Handle failure reading ifzd"

let read_header ifzd =
  let ifhd_bytes = (string_to_bytes "IFhd")
  let chunks = read_ifzd_chunks ifzd in
  let ifhd = find_record chunks ifhd_bytes in
  match ifhd with
  | Some (Record [
                  Header ifhd_bytes;
                  Length _;
                  Integer16 (Some release_number);
                  ByteString (Some serial_number, 6);
                  Integer16 (Some checksum);
                  Integer24 (Some pc) ]) ->
    ((Release_number release_number), (Serial_number (string serial_number)), (Checksum checksum), pc)
  | _ -> failwith "TODO handle failure reading ifhd"

let read_stacks ifzd =
  let stks_bytes = string_to_bytes "Stks"
  let chunks = read_ifzd_chunks ifzd in
  let stacks_chunk = find_record chunks stks_bytes in
  match stacks_chunk with
  | Some (Record [
                  Header stks_bytes;
                  Length _;
                  UnsizedList items ]) -> items
  | _ -> failwith "TODO handle failure reading stacks"

let read_memory ifzd =
  let cmem_bytes = string_to_bytes "CMem"
  let umem_bytes = string_to_bytes "UMem"
  let chunks = read_ifzd_chunks ifzd in
  let cmem_chunk = find_record chunks cmem_bytes in
  let umem_chunk = find_record chunks umem_bytes in
  let compressed = match cmem_chunk with
                    | None -> None
                    | Some (Record [
                                    Header cmem_bytes;
                                    Length (Some length);
                                    RemainingBytes (Some bytes)]) ->
                      Some (Compressed bytes)
                    | _ -> failwith "TODO: Handle failure reading CMem" in
  let uncompressed = match umem_chunk with
                        | None -> None
                        | Some (Record [
                                        Header umem_bytes;
                                        Length (Some length);
                                        RemainingBytes (Some bytes)]) ->
                          Some (Uncompressed bytes)
                        | _ -> failwith "TODO: Handle failure reading UMem" in
  (compressed, uncompressed)
