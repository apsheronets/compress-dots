
open ExtLib

(*
let lines file =
  let chan = open_in file in
  Stream.from
    (fun _ ->
      try Some (input_line chan)
      with End_of_file -> close_in chan; None)
*)

let input_lines =
  Stream.from
    (fun _ ->
      try Some (read_line ())
      with End_of_file -> None)

let stream_map f stream =
  Stream.from
    (fun _ ->
      try Some (f (Stream.next stream))
      with Stream.Failure -> None)

let sep = " "

let values stream =
  stream_map (fun s -> String.nsplit s sep) stream

let compress_stream stream =
  let prev_values = ref [] in
  Stream.from
    (fun _ ->
      try
        match !prev_values with
        | [] ->
            let values = Stream.next stream in
            let () = prev_values := values in
            Some values
        | h1::t1 ->
            let rec loop n =
              let values = Stream.peek stream in
              match values with
              | Some ((h2::t2) as values) ->
                  if t1 = t2
                  then
                    let () = prev_values := values in
                    let () = Stream.junk stream in
                    loop (succ n)
                  else
                    if n = 0
                    then
                      let values = Stream.next stream in
                      let () = prev_values := values in
                      Some values
                    else
                      let r = Some (!prev_values) in
                      let () = prev_values := [] in
                      r
              | Some [] -> assert false
              | None ->
                  let r = Some (!prev_values) in
                  let () = prev_values := [] in
                  r in
            loop 0
      with Stream.Failure ->
        match !prev_values with
        | [] -> None
        | x -> prev_values := []; Some x
    )

let print_values stream =
  Stream.iter (fun l -> print_endline (String.concat " " l)) stream

let () =
  let lines = input_lines in
  let values = values lines in
  let compressed = compress_stream values in
  print_values compressed
