(** Type definition of Regular Expression Abstract Syntax Tree *)

module String_set = Set.Make (String)

type re =
  | Literal of string (* a ∈ Σ *)
  | Epsilon           (* ε *)
  | Empty             (* ∅ *)
  | Union of re * re  (* E + R *)
  | Concat of re * re (* E·R *)
  | Star of re        (* E* *)
  | Wildcard of String_set.t
