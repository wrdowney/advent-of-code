open Core

let read_lines file =
  Stdio.In_channel.with_file file ~f:(fun chan ->
    let x = In_channel.input_all chan in
    String.split_lines x)
;; 

let split_first s =
  let a = String.split ~on:' ' s in
  List.hd_exn a
;;

let square x = x *. x

let num_filter = Re.Pcre.re {|\d+|} |> Re.compile ;;

let time f =
  let t = Core_unix.gettimeofday () in
  let res = f () in
  Printf.printf "Execution time: %f seconds\n"
                (Core_unix.gettimeofday () -. t);
  res
;;
