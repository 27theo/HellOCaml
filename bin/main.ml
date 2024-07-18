open! Base

(* Possible exit codes *)
module Exit_codes = struct
  let success = 0
  let no_input = 1
end

(* Print usage help *)
let print_help () =
  Stdio.print_endline "Commands and usage:";
  List.map
    [
      "    help               - Display this output";
      "    show               - Show TODO list";
      "    add <note:str>     - Add note to TODO list";
      "    remove <index:int> - Remove note at index <index> from TODO list";
      "    save <path:str>    - Persist the TODO list in the filesystem";
      "    load <path:str>    - Load a TODO list from the filesystem at path";
      "    quit               - Quit the program";
    ]
    ~f:Stdio.print_endline
  |> ignore

(* Display todo list *)
let show_todo todo =
  Stdio.print_endline "# | TODO";
  match List.length todo with
  | 0 -> Stdio.print_endline "  | (No entries)"
  | _ -> List.mapi todo ~f:(fun i s -> Stdio.printf "%i | %s\n" i s) |> ignore

(* Add to todo list *)
let add_todo todo note = note :: todo

(* Remove from todo list by index *)
let remove_todo todo n = List.filteri todo ~f:(fun i _ -> not (equal_int i n))

(* Save todo to a file *)
let save_todo todo path =
  let oc = Out_channel.open_text path in
  List.map todo ~f:(Stdio.Out_channel.fprintf oc "%s\n") |> ignore;
  Stdio.Out_channel.close_no_err oc;
  ()

(* Load todo from a file *)
let load_todo path = Stdio.In_channel.read_lines path ~fix_win_eol:true

(* Main *)
let () =
  Stdio.print_endline "Welcome to: todo v1.0";
  print_help ();
  Stdio.print_endline "";
  (* Our list! *)
  let todo = ref [] in
  while true do
    Stdio.printf "> %!" (* %! flushes stdout *);
    let user_input =
      match Stdio.In_channel.input_line Stdio.stdin ~fix_win_eol:true with
      | Some s -> s
      | None -> Stdlib.exit Exit_codes.no_input (* Crash out *)
    in
    let argv = String.split user_input ~on:' ' in
    (* Perform all our command parsing here, and delegate operations to simple
       functions. Main benefit - all dereferencing of todo and mutation happens
       in main, keeping other functions entirely pure. *)
    match argv with
    | [ "help" ] -> print_help ()
    | [ "show" ] -> show_todo !todo
    | "add" :: _ -> (
        match String.drop_prefix user_input 4 |> String.strip with
        | "" -> Stdio.print_endline "Usage: add <note:str>"
        | note ->
            todo := add_todo !todo note;
            Stdio.printf "Added todo: %s\n" note)
    | [ "remove"; s ] -> (
        match Int.of_string_opt s with
        | Some i ->
            todo := remove_todo !todo i;
            Stdio.printf "Entry %i removed\n" i
        | _ -> Stdio.print_endline "Usage: remove <index:int>")
    | [ "save"; s ] -> (
        try
          save_todo !todo s;
          Stdio.printf "Saved todo at %s\n" s
        with e -> Exn.to_string e |> Stdio.printf "Error: %s\n")
    | [ "load"; s ] -> (
        try
          todo := load_todo s;
          Stdio.printf "Loaded todo from %s\n" s
        with e -> Exn.to_string e |> Stdio.printf "Error: %s\n")
    | [ "quit" ] | [ "q" ] ->
        Stdio.print_endline "Goodbye!";
        Stdlib.exit Exit_codes.success
    | [ "" ] -> ()
    | _ -> Stdio.printf "Invalid syntax: %s\n" user_input
  done
