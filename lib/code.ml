open Base
open Stdint

let ( let@ ) res f = Base.Option.bind res ~f

type t = OpConstant [@@deriving enum, show]
type instructions = t list [@@deriving show]

let to_byte enum = to_enum enum |> Uint8.of_int
let of_byte byte = of_enum @@ Uint8.to_int byte

type definition =
  { name : string
  ; op_width : int list
  }

let lookup = function
  | OpConstant -> "OpConstant", [ 2 ]
;;

let u16_to_u8s u16 =
  let bytes = Bytes.make 2 '0' in
  Uint16.to_bytes_big_endian u16 bytes 0;
  Bytes.to_list bytes |> List.map ~f:(fun a -> Char.to_int a |> Uint8.of_int)
;;

exception BadWidth of int

let make op ops =
  let _, args = lookup op in
  let instructions = [ to_byte op ] in
  List.foldi ops ~init:instructions ~f:(fun i acc op ->
    let width = List.nth_exn args i in
    match width with
    | 2 -> acc @ u16_to_u8s @@ Uint16.of_int op
    | _ -> raise @@ BadWidth width)
;;

type make_test =
  { op : t
  ; operands : int list
  ; expected : uint8 list
  }

let%test "testmake" =
  let tests =
    [ { op = OpConstant
      ; operands = [ 65534 ]
      ; expected = [ to_byte OpConstant; Uint8.of_int 255; Uint8.of_int 254 ]
      }
    ]
  in
  List.fold tests ~init:true ~f:(fun acc { op; operands; expected } ->
    if acc
    then (
      let instr = make op operands in
      List.equal (fun a b -> Uint8.to_int a = Uint8.to_int b) instr expected)
    else false)
;;
