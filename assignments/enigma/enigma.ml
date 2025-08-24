(* Simple Enigma Machine - Everything in one file *)



(* Import configuration *)
let config = Config.config

(* Step 2: Plugboard *)
let apply_plugboard plugboard c =
  let rec find_swap = function
    | [] -> c
    | (a, b) :: rest ->
        if c = a then b
        else if c = b then a
        else find_swap rest
  in
  find_swap plugboard

(* Rotors and reflector *)
let rotor_wiring = [
  "EKMFLGDQVZNTOWYHXUSPAIBRCJ";  (* Rotor I *)
  "AJDKSIRUXBLHWTMCQGZNPYFVOE";  (* Rotor II *)
  "BDFHJLCPRTXVZNYEIWGAKMUSQO";  (* Rotor III *)
]

let reflector = "YRUHQSLDPXNGOKMIEBFZCWVJAT"  (* Reflector B *)

let rotor_forward rotor_num position ring_setting input =
  let wiring = List.nth rotor_wiring (rotor_num - 1) in
  let adjusted_pos = (input + position - ring_setting + 26) mod 26 in
  let output_char = String.get wiring adjusted_pos in
  let output = Char.code output_char - Char.code 'A' in
  (output - position + ring_setting + 26) mod 26

let rotor_backward rotor_num position ring_setting input =
  let wiring = List.nth rotor_wiring (rotor_num - 1) in
  let adjusted_pos = (input + position - ring_setting + 26) mod 26 in
  let output_char = Char.chr (adjusted_pos + Char.code 'A') in
  let output = String.index wiring output_char in
  (output - position + ring_setting + 26) mod 26

let apply_reflector input =
  let output_char = String.get reflector input in
  Char.code output_char - Char.code 'A'

(* Update rotors *)
let notches = ['Q'; 'E'; 'V']  (* Notch positions for each rotor *)

let advance_rotors rotors positions =
  let rec advance rotors positions should_advance_next =
    match rotors, positions with
    | [r], [p] -> [r], [(p + 1) mod 26]
    | r :: rs, p :: ps ->
        let notch_pos = Char.code (List.nth notches (r - 1)) - Char.code 'A' in
        let should_advance = should_advance_next || (p = notch_pos) in
        let new_rs, new_ps = advance rs ps should_advance in
        let new_p = if should_advance_next || should_advance then (p + 1) mod 26 else p in
        r :: new_rs, new_p :: new_ps
    | _ -> rotors, positions
  in
  advance rotors positions false

let encrypt_char rotors positions ring_settings plugboard c =
  if not (Char.uppercase_ascii c >= 'A' && Char.uppercase_ascii c <= 'Z') then c
  else
    let c = Char.uppercase_ascii c in
    let c = apply_plugboard plugboard c in
    let input = Char.code c - Char.code 'A' in
    let new_rotors, new_positions = advance_rotors rotors positions in
    let result = List.fold_left2 (fun acc rotor pos ->
      let ring = List.nth ring_settings (rotor - 1) in
      rotor_forward rotor pos ring acc) input new_rotors new_positions in
    let result = apply_reflector result in
    let result = List.fold_left2 (fun acc rotor pos ->
      let ring = List.nth ring_settings (rotor - 1) in
      rotor_backward rotor pos ring acc) result (List.rev new_rotors) (List.rev new_positions) in
    let output_char = Char.chr (result + Char.code 'A') in
    apply_plugboard plugboard output_char

let encrypt_string config text =
  let rec encrypt_chars rotors positions acc = function
    | [] -> String.concat "" (List.rev acc)
    | c :: cs ->
        let encrypted = encrypt_char rotors positions config.ring_settings config.plugboard c in
        let new_rotors, new_positions = advance_rotors rotors positions in
        encrypt_chars new_rotors new_positions (String.make 1 encrypted :: acc) cs
  in
  let chars = List.init (String.length text) (String.get text) in
  encrypt_chars config.rotors config.positions [] chars

let decrypt_string config text = encrypt_string config text  (* Enigma is symmetric *)

(* File I/O functions *)
let read_file path =
  let ic = open_in path in
  let rec read_lines acc =
    try
      let line = input_line ic in
      read_lines (line :: acc)
    with End_of_file ->
      close_in ic;
      String.concat "\n" (List.rev acc)
  in
  read_lines []

let write_file path content =
  let oc = open_out path in
  output_string oc content;
  close_out oc

let process_file input_path output_path config encrypt =
  let content = read_file input_path in
  let processed = if encrypt then
    encrypt_string config content
  else
    decrypt_string config content
  in
  write_file output_path processed

let process_input_folder input_dir output_dir config encrypt =
  let input_path = Filename.concat input_dir "challenge.txt" in
  let output_path = Filename.concat output_dir "challenge.txt" in
  if Sys.file_exists input_path then
    process_file input_path output_path config encrypt

let usage () =
  Printf.printf "Usage: enigma [encrypt|decrypt]\n";
  Printf.printf "  encrypt - encrypt challenge.txt\n";
  Printf.printf "  decrypt - decrypt challenge.txt\n"

let main () =
  let argc = Array.length Sys.argv in
  
  if argc < 2 then (
    usage ();
    exit 1
  );
  
  let mode = Sys.argv.(1) in
  
  try
    
    if not (Sys.file_exists "output") then Sys.mkdir "output" 0o755;
    if not (Sys.file_exists "decrypted") then Sys.mkdir "decrypted" 0o755;
    
    match mode with
    | "encrypt" ->
        if not (Sys.file_exists "input") then (
          Printf.printf "Error: input/ folder does not exist\n";
          exit 1
        );
        process_input_folder "input" "output" config true;
        Printf.printf "Encryption complete!\n"
        
    | "decrypt" ->
        if not (Sys.file_exists "output") then (
          Printf.printf "Error: output/ folder does not exist\n";
          exit 1
        );
        process_input_folder "output" "decrypted" config false;
        Printf.printf "Decryption complete!\n"
        
    | _ ->
        Printf.printf "Error: Invalid mode '%s'\n" mode;
        usage ();
        exit 1
        
  with e ->
    Printf.printf "Error: %s\n" (Printexc.to_string e);
    exit 1

let () = main ()
