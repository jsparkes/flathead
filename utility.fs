module Utility

open Type
(* Just some useful stuff that has no good home *)

// Use Map<int, int> in C#
//module IntMap = Map.Make(struct type t = int let compare = compare end)

let byte_of_int x =
  x &&& 0xff

let string_of_char (x : char) =
  string(x)

// Not using byte type for greater compatibility with OCaml
let string_of_byte (b : int) =
  string_of_char (char b)

let char_of_int n =
  char n

let int_of_char (c : char) =
  int c

let truncate text length =
  if (String.length text) > length then text.Substring(0, length)
  else text

let rec times f n item =
  if n = 0 then item else times f (n - 1) (f item)

let spaces n =
  string (Array.create n ' ')

let rec reverse_index_from (text : string) target index =
  if index < 0 then None
  else if text.[index] = target then Some index
  else reverse_index_from text target (index - 1)

let left_string (text : string) length =
  text.Substring(0, length)

let right_string (text : string) index =
  text.Substring(index, ((String.length text) - index))

let break_string (text : string) (target : char) =
  let index = text.IndexOf(target)
  let left = left_string text index in
  let right = right_string text (index + 1) in
  (left, right)

let replace_at original_text index new_text =
  let len = String.length new_text in
  let left = left_string original_text index in
  let right = right_string original_text (index + len) in
  left + new_text + right

let accumulate_strings to_string items =
  let folder text item =
    text + (to_string item) in
  List.fold folder "" items

let accumulate_strings_loop to_string start max =
  let rec aux acc i =
    if i >= max then acc
    else aux (acc + (to_string i)) (i + 1) in
  aux "" start

let unsigned_word word =
  ((word % 65536) + 65536) % 65536

let signed_word word =
  let canonical = unsigned_word word in
  if canonical > 32767 then canonical - 65536 else canonical

  (* Helper method that takes an item and a function that produces related items.
     The result is the transitive closure of the relation. *)

let transitive_closure_many items relation =
  let rec merge related set stack =
    match related with
    | [] -> (set, stack)
    | head :: tail ->
      if Set.contains head set then merge tail set stack
      else merge tail (Set.add head set) (head :: stack) in
  let rec aux set stack =
    match stack with
    | [] -> set
    | head :: tail ->
      let (new_set, new_stack) = merge (relation head) set tail in
      aux new_set new_stack in
  aux Set.empty items

let transitive_closure item relation =
  transitive_closure_many [item] relation

let reflexive_closure_many items relation =
  let t = transitive_closure_many items relation in
  List.fold (fun s i -> if Set.contains i s then s else Set.add i s) t items

let reflexive_closure item relation =
  reflexive_closure_many [item] relation



let bit0 = Bit_number 0
let bit1 = Bit_number 1
let bit2 = Bit_number 2
let bit3 = Bit_number 3
let bit4 = Bit_number 4
let bit5 = Bit_number 5
let bit6 = Bit_number 6
let bit7 = Bit_number 7
let bit8 = Bit_number 8
let bit9 = Bit_number 9
let bit10 = Bit_number 10
let bit11 = Bit_number 11
let bit12 = Bit_number 12
let bit13 = Bit_number 13
let bit14 = Bit_number 14
let bit15 = Bit_number 15

type bit_size =
  Bit_size of int

let size1 = Bit_size 1
let size2 = Bit_size 2
let size3 = Bit_size 3
let size4 = Bit_size 4
let size5 = Bit_size 5
let size6 = Bit_size 6
let size7 = Bit_size 7

let fetch_bit (Bit_number n) word =
  (word &&& (1 <<< n)) >>> n = 1

let clear_bit (Bit_number n) word =
  word &&& (~~~ (1 >>> n))

let set_bit (Bit_number n) word =
  word ||| (1 <<< n)

let set_bit_to n word value =
  if value then set_bit n word
  else clear_bit n word

let fetch_bits (Bit_number high) (Bit_size length) word =
  let mask = ~~~ (-1 <<< length) in
  (word >>> (high - length + 1)) &&& mask

let display_bytes get_byte first length =
  let blocksize = 16 in
  let to_string i =
    let header =
      if i % blocksize = 0 then Printf.sprintf "\n%06x: " i
      else "" in
    let byte = get_byte (Byte_address i) in
    let contents = Printf.sprintf "%02x " byte in
    header + contents in
  (accumulate_strings_loop to_string first (first + length)) + "\n"

let get_file filename =
  string(System.IO.File.ReadAllBytes(filename))

let write_file filename (text : string) =
  //System.IO.File.WriteAllBytes(filename, System.Text.UTF8Encoding.UTF8.GetBytes(text))
  System.IO.File.WriteAllBytes(filename, System.Text.ASCIIEncoding.ASCII.GetBytes(text))

let rec first_match items predicate =
  match items with
  | h :: t -> if predicate h then Some h else first_match t predicate
  | [] -> None

(* Binary search a range. Min is inclusive, max is exclusive. *)
let rec binary_search min max compare =
  if min >= max then
    None
  else
    let middle = (min + max) / 2 in
    let comparison = compare middle in
    if comparison < 0 then binary_search (middle + 1) max compare
    else if comparison > 0 then binary_search min middle compare
    else Some middle

let max x y =
  if x > y then x else y

let min x y =
  if x < y then x else y

let word_size = 2

let inc_byte_addr_by (Byte_address address) offset =
  Byte_address (address + offset)

let dec_byte_addr_by address offset =
  inc_byte_addr_by address (0 - offset)

let inc_byte_addr address =
  inc_byte_addr_by address 1

let inc_word_addr_by (Word_address address) offset =
  Word_address (address + offset * word_size)

let inc_word_addr address =
  inc_word_addr_by address 1

let byte_of_string (String_address address) offset =
  Byte_address (address + offset)

let string_of_sz (Sz_address address) =
  String_address address

let string_of_wps (Word_prefixed_string wps) =
  String_address (wps + word_size)

let length_addr_of_wps (Word_prefixed_string wps) =
  Word_address wps

let string_of_bps (Byte_prefixed_string bps) =
  String_address (bps + 1)

let length_addr_of_bps (Byte_prefixed_string bps) =
  Byte_address bps

let is_in_range (Byte_address address) size =
    0 <= address && address < size

let is_out_of_range address size =
    not (is_in_range address size)

let dereference_string address bytes =
  if is_out_of_range address (String.length bytes) then
    failwith "address out of range"
  else
    let (Byte_address addr) = address in
    int bytes.[addr]

let address_of_high_byte (Word_address address) =
  Byte_address address

let address_of_low_byte (Word_address address) =
  Byte_address (address + 1)

let byte_addr_to_word_addr (Byte_address address) =
  Word_address address

type List<'a> with
  static member fold_left a b c = List.fold a b c

let (mod) a b = a % b

let (land) a b = a &&& b
let (lor) a b = a ||| b

let (lsl) a b = a <<< b
let (lsr) a b = a >>> b

// Is this correct?
let (asr) a b = a >>> b