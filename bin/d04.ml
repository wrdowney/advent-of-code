open Core
open Advent

let pattern = Re.Pcre.re {|mul\(\d+,\d+\)|} |> Re.compile
let pattern2 = Re.Pcre.re {|do\(\)|don't\(\)|mul\(\d+,\d+\)|} |> Re.compile

let multiply text =
  let x = Re.matches num_filter text |> List.map ~f:int_of_string in 
  match x with
  | [] | [ _ ] -> 0
  | [ a; b ] -> a * b
  | a :: b :: _ -> a * b
;;

let rec multiply2 text should_mul sum = 
  match text with
  | [] -> sum
  | [ str ] -> sum + multiply str
  | str :: rest ->
    if String.equal str "do()"
    then multiply2 rest true sum
    else if String.equal str "don't()"
    then multiply2 rest false sum
    else if should_mul
    then multiply2 rest true sum + multiply str
    else multiply2 rest false sum

let run () =
  let line = read_lines "input3" |> String.concat ~sep:" " in
  let matches = Re.matches pattern line |> List.map ~f:multiply in
  let matches2 = Re.matches pattern2 line in
  let res = List.fold matches ~init:0 ~f:( + ) in
  let res2 = multiply2 matches2 true 0 in
  Printf.printf "Part1: %i Part2: %i\n" res res2

let () =
  time run
;;