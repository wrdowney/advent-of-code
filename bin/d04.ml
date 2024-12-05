open Core
open Advent
let out_of_bounds row col rows cols = 
  row < 0 || row >= rows || col < 0 || col >= cols
;;

let xmas_finder input =
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


let locations_of c input = 
  let find_row r_idx row = 
    let row_len = String.length row in
    let rec find_col c_idx sum = 
      if c_idx >= row_len then sum
      else (
        let sum = if Char.equal row.[c_idx] c then (r_idx, c_idx) :: sum else sum in
        find_col (c_idx + 1) sum
      )
    in
    find_col 0 []
  in
  let rec find_rows r_idx rows sum = 
    match rows with
    | [] -> List.rev sum
    | row :: rest ->
      let l = find_row r_idx row in
      find_rows (r_idx + 1) rest (l @ sum)
    in
  find_rows 0 input []
;;

let x_finder mlocs alocs slocs =
  let offsets = [ (-1, 1), (1, -1); (1, 1), (-1, -1); (-1, -1), (1, 1); (1, -1), (-1, 1) ] in
  List.fold alocs ~init:0 ~f:(fun sum cord -> 
    if List.fold offsets ~init:0 ~f:(fun sum2 (m_off, s_off) ->
      if List.mem mlocs (add_pairs m_off cord) ~equal:pairs_equal &&
        List.mem slocs (add_pairs s_off cord) ~equal:pairs_equal then sum2 + 1 else sum2) 
    = 2 then sum + 1
else sum)

let run () =
  let lines = read_lines "inputs/input4" in
  let res = xmas_finder lines in
  let mlocs = locations_of 'M' lines in
  let alocs = locations_of 'A' lines in
  let slocs = locations_of 'S' lines in
  let res2 = x_finder mlocs alocs slocs in  
  Printf.printf "Part1: %i Part2: %i\n" res res2 
;;

let () =
  time run
;;