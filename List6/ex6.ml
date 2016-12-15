open Str

type config = {
  file : string;
  regexp : string
}

type line = {
  number : int;
  content : string;
}

let read_args () : config =
  {file = Sys.argv.(1); regexp = Sys.argv.(2) }

let read_file path =
  let lines = ref [] in
  let ic = open_in path in
  let rec loop counter =
    try
      lines := {number = counter; content = input_line ic} :: !lines;
      loop (counter + 1)
    with End_of_file -> close_in ic; !lines
  in
  loop 1

let print_line l =
begin
  print_int l.number;
  print_endline (":" ^ l.content);
end

let rec print_results l =
  match l with
    | x::xs -> (print_line x;
                if read_line () = "c" then () else print_results xs)
    | [] -> ()

let match_line cfg l =
  let re = regexp cfg.regexp in
  try
    let _ = search_forward re l.content 0 in true
  with Not_found -> false

let main () =
  let cfg = read_args () in
  let lines = read_file cfg.file in
  print_results (List.filter (match_line cfg) lines)

let _ = main ()
