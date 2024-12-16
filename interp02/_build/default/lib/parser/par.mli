
(* The type of tokens. *)

type token = 
  | VAR of (string)
  | UNITTY
  | UNIT
  | TRUE
  | THEN
  | SUB
  | RPAREN
  | REC
  | OR
  | NUM of (int)
  | NEQ
  | MUL
  | MOD
  | LTE
  | LT
  | LPAREN
  | LET
  | INTTY
  | IN
  | IF
  | GTE
  | GT
  | FUN
  | FALSE
  | EQ
  | EOF
  | ELSE
  | DIV
  | COLON
  | BOOLTY
  | ASSERT
  | ARROW
  | AND
  | ADD

(* This exception is raised by the monolithic API functions. *)

exception Error

(* The monolithic API. *)

val prog: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Utils.prog)
