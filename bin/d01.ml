open Advent
let count_occurences arr x =
  let count = ref 0 in 
  Array.iter (fun y -> if y = x then incr count) arr;
  !count

let parse filename =
  let left_side = ref [] in
  let right_side = ref [] in
  
  let ic = open_in filename in
  try
    while true do 
      let line = input_line ic in
        let parts = String.split_on_char ' ' line |> List.filter (fun s -> s <> "") in
        left_side := (List.hd parts) :: !left_side;
        right_side := (List.tl parts @ !right_side);
    done;
  with End_of_file -> close_in ic;
  
  (Array.of_list (List.rev !left_side), Array.of_list (List.rev !right_side))

let run () = 
  let argc = Array.length Sys.argv in
  if argc = 2 then
    let left_array, right_array = parse Sys.argv.(1) in
    Array.sort compare left_array;
    Array.sort compare right_array;

    (* merge lists*)

    (*take difference of every element at index and sum*)
    let sum = ref 0 in
    for i = 0 to (Array.length left_array) - 1 do
      sum := !sum + abs ((int_of_string left_array.(i)) - (int_of_string right_array.(i)));
    done;

    let sum2 = ref 0 in
    Array.iter (fun x -> 
      let right_array_int = Array.map int_of_string right_array in
      sum2 := !sum2 + (x * (count_occurences right_array_int x));
      Printf.printf "x: %s occr: %d\n" (string_of_int x) (count_occurences right_array_int x);
    ) (Array.map int_of_string left_array);
    Printf.printf "Sum: %d\n" !sum;
    Printf.printf "Sum2: %d\n" !sum2;
  else
    Printf.printf "Usage: %s <filename>\n" Sys.argv.(0)
  ;;

let () = 
  time run
;;