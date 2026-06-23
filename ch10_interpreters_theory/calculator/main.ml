open Ast

(**[parse s] parses [s] into an AST. *)
let parse (s : string) : expr =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  ast
(**How does this work?
  1) some string s passed in as argument
  2) A lexer buffer based on s is created using the Lexing.from_string function
  3) The buffer is then passed to our defined read function in the Lexer which
  takes the buffer, and creates the relevant token needed one at a time at the Parser's call
  4) The Parser drives the calling, in which inside the prog, we have a rule
  on how to build a tree for expr. So for INT tokens, we return an 
  AST Node with value Int x. The Parser does this per token in the buffer
   *)


(**[string_of_val e] is [e] in string format.
   Requires: [e] is a value *)
let string_of_val e =
  match e with
  | Int x -> string_of_int x
  | Binop _ -> failwith "Precondition violated"

(**[is_value e] is true IFF [e] is a value. *)
let is_value e =
  match e with
  | Int _ -> true
  | Binop _ -> false

let extract e =
  match e with
  | Int x -> x
  | Binop _ -> failwith "cannot extract binary operation"

(**[step e] takes a single step in the evaluation of [e]*)
let rec step : expr -> expr = function
  | Int _ -> failwith "Does not step"
  | Binop (x, e1, e2) -> 
    match (is_value e1, is_value e2) with
    | true, false -> step (Binop (x, e1, step e2))
    | false, _ -> step (Binop (x, step e1, e2))
    | true, true -> 
      if x = Add then Int (extract e1 + extract e2)
      else Int (extract e1 * extract e2)

(**[eval e] fully evaluates [e] to a value [v].*)
let rec eval e =
  if is_value e then e else
  e |> step |> eval

let interp s = 
  s |> parse |> eval |> string_of_val
