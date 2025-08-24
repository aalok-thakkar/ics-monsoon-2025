(* Enigma Configuration *)
(* This is the secret configuration that needs to be broken! *)

(* Configuration type *)
type config = {
  rotors: int list;
  positions: int list;
  ring_settings: int list;
  plugboard: (char * char) list;
}

(* Fixed rotor order: I, II, III *)
let rotors = [1; 2; 3]

(* Starting positions (3 letters to find - this is what you need to break!) *)
let positions = [0; 1; 2]  (* ABC *)

(* Fixed ring settings: A-A-A *)
let ring_settings = [0; 0; 0]

(* Plugboard connections (up to 2 swaps to find) *)
let plugboard = [('A', 'Z'); ('B', 'D')]

(* Create the config record *)
let config = {
  rotors = rotors;
  positions = positions;
  ring_settings = ring_settings;
  plugboard = plugboard;
}
