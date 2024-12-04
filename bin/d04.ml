open Core
open Advent
let out_of_bounds row col rows cols = 
  row < 0 || row >= rows || col < 0 || col >= cols
;;

let count_occurences input =
  let word = "XMAS" in
  let rows = List.length input in
  let cols = String.length (List.hd_exn input) in

  let search_point row col =
    let offsets = [(-1, -1); (-1, 0); (-1, 1); (0, -1); (0, 1); (1, -1); (1, 0); (1, 1)] in
    let rec search_offset r c offset depth = 
      let row_offset, col_offset = offset in
      if depth = 4 then true
      else if out_of_bounds r c rows cols then false
      else if not (Char.equal(List.nth_exn input r).[c]  word.[depth]) then false
      else search_offset (r + row_offset) (c + col_offset) offset (depth + 1)
    in
    List.fold offsets ~init:0 ~f:(fun sum offset ->
      if search_offset row col offset 0 then sum + 1 else sum)
  in
  let rec search row col sum = 
    if row >= rows then sum
    else if col >= cols then search (row + 1) 0 sum
    else search row (col + 1) (sum + search_point row col)
  in
  search 0 0 0
;;


let run () =
  let line = read_lines "inputs/input4" in
  let res = count_occurences line in
  Printf.printf "Part1: %i \n" res
;;

let () =
  time run
;;