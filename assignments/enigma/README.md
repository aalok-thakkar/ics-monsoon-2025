# Breaking the Enigma Machine

## The Enigma Challenge

During World War II, the German military used the Enigma machine to encrypt their communications. This electromechanical device created what was thought to be unbreakable encryption, allowing secure communication between military units, submarines, and command centers.

The Allies' ability to break Enigma codes was crucial to the war effort. At Bletchley Park in England, Alan Turing and his team developed the Bombe machine - a device that could systematically test possible Enigma configurations to find the correct settings used for encryption.

## Your Mission: Break the Enigma

Your task is to implement a code-breaking system that can decrypt messages without knowing the Enigma settings. This is exactly what Turing and his team accomplished at Bletchley Park.

### What It Means to "Break" Enigma

Breaking Enigma means finding the correct configuration settings that were used to encrypt a message, allowing you to decrypt it. The settings include:

1. **Rotor Starting Positions** (3 letters, e.g., "ABC")
2. **Plugboard Swaps** (up to 2 letter pairs, e.g., A↔Z, B↔D)

### The Cryptographic Challenge

A naive brute-force approach would require testing:
- All rotor positions: 26³ = 17,576 combinations
- All possible plugboard swaps: ~325 combinations
- **Total: ~5.7 million configurations**

While this is feasible with modern computers, the real challenge is to implement **Turing's Bombe logic** - a smarter approach that discards impossible configurations early, dramatically reducing the search space.

## How to Use This Tool

### Setup

1. **Run the Enigma machine:**
   ```bash
   # Option 1: Using the script
   ./run.sh encrypt
   
   # Option 2: Direct OCaml execution
   ocaml config.ml enigma.ml encrypt
   ```

2. **The configuration is in a separate file** `config.ml`:
   ```ocaml
   let config = {
     rotors = [1; 2; 3];
     positions = [0; 1; 2];
     ring_settings = [0; 0; 0];
     plugboard = [('A', 'Z'); ('B', 'D')];
   }
   ```

### Encryption (Simulating the Enemy)

```bash
# Encrypt messages using the secret settings
./run.sh encrypt
```

This simulates what the German military would do - encrypt messages using their secret Enigma configuration.

### Decryption (When You Know the Settings)

```bash
# Decrypt messages when you know the correct settings
./run.sh decrypt
```

This is what happens when you successfully break the code and know the correct configuration.

## The Real Challenge: Code Breaking

The true assignment is to implement a **code-breaking system** that can:

1. **Take an encrypted message** (without knowing the settings)
2. **Systematically test configurations** to find the correct one
3. **Use intelligent heuristics** to discard impossible configurations early
4. **Successfully decrypt the message** by finding the right settings

### Implementation Strategy

Instead of brute force, implement logic similar to Turing's Bombe:

1. **Analyze letter frequencies** in the encrypted text
2. **Use known plaintext patterns** (like "HEIL HITLER" at the end of messages)
3. **Test rotor positions systematically** based on likely patterns
4. **Eliminate impossible plugboard combinations** early
5. **Use cribs** (known plaintext-ciphertext pairs) to narrow down possibilities

### Your Enigma Implementation

This tool provides a simplified Enigma with:

- **3 rotors** in fixed order (I, II, III)
- **Fixed ring settings** (A-A-A)
- **Fixed reflector** (Reflector B)
- **Up to 2 plugboard swaps** (e.g., A↔Z, B↔D)
- **Configurable starting positions** (3 letters)

The `config` type represents:
```ocaml
type config = {
  rotors: int list;           (* Fixed: [1,2,3] *)
  positions: int list;        (* 3 letters to find *)
  ring_settings: int list;    (* Fixed: [0,0,0] *)
  plugboard: (char * char) list; (* Up to 2 swaps to find *)
}
```

## Historical Context

The breaking of Enigma was one of the most significant achievements in cryptography and computer science. It involved:

- **Alan Turing's** mathematical insights and machine design
- **Gordon Welchman's** diagonal board improvements
- **Thousands of people** working at Bletchley Park
- **The development of early computers** and cryptanalysis techniques

Your challenge is to recreate this breakthrough using modern programming techniques and computational power.

## Your First Challenge

1. **Encrypt a message** using the current settings:
   ```bash
   ./run.sh encrypt
   ```

2. **Look at the encrypted output** in `output/challenge.txt` - this is what you need to break!

3. **Your task**: Write a program that can find the correct settings (positions=ABC, plugboard=AZ,BD) just by looking at the encrypted text.

4. **Hint**: Start with a brute-force approach, then optimize it using frequency analysis and known patterns.

## Getting Started

1. **Understand the Enigma mechanism** by studying the code in `enigma.ml`
2. **Implement a brute-force approach** first to understand the problem
3. **Add intelligent heuristics** to reduce the search space
4. **Test with known plaintext-ciphertext pairs**
5. **Optimize your algorithm** to break codes efficiently

Remember: The goal isn't just to decrypt messages, but to understand and implement the cryptographic principles that made breaking Enigma possible.

Good luck, code breaker!
