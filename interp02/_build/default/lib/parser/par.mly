%{
open Utils

let rec mk_app e = function
  | [] -> e
  | x :: es -> mk_app (SApp (e, x)) es

let concat e1 e2 = e1::e2
%}

%token <int> NUM
%token <string> VAR
%token UNIT
%token TRUE
%token FALSE
%token LPAREN
%token RPAREN
%token ADD
%token SUB
%token MUL
%token DIV
%token MOD
%token LT
%token LTE
%token GT
%token GTE
%token EQ
%token NEQ
%token AND
%token OR
%token IF
%token THEN
%token ELSE
%token LET
%token IN
%token FUN
%token REC
%token COLON
%token ARROW
%token ASSERT


%token INTTY
%token BOOLTY
%token UNITTY

%token EOF

%right ARROW
%right OR
%right AND
%left LT LTE GT GTE EQ NEQ
%left ADD SUB
%left MUL DIV MOD

%start <Utils.prog> prog


%%

prog:
  | toplets = toplet* EOF { toplets }

toplet:
  | LET ;x = VAR; y=arg* ; COLON ; ty = ty ; EQ; e = expr { { is_rec = false; name = x; args = y; ty = ty; value = e} }
  | LET ;REC; x = VAR ; y = arg ; y2 = arg* ; COLON ;  ty = ty ;EQ; e = expr { { is_rec = true; name = x; args = concat y y2; ty = ty; value = e} }

arg:
| LPAREN ; x = VAR ; COLON ; ty=ty ; RPAREN {(x,ty)}

ty:
  | INTTY { IntTy }
  | BOOLTY { BoolTy }
  | UNITTY {UnitTy}
  | ty = ty ; ARROW ; ty1 = ty {FunTy (ty,ty1)}
  | LPAREN ; ty = ty ; RPAREN { ty }

expr:
  | IF; e1 = expr; THEN; e2 = expr; ELSE; e3 = expr { SIf (e1, e2, e3) }
  | LET; x = VAR; y=arg*; COLON ; ty=ty ; EQ; e1 = expr; IN; e2 = expr { SLet {is_rec = false; name = x; args = y; ty=ty; value = e1; body = e2} }
  | LET ; REC ; x=VAR ; y = arg ; y1 = arg*; COLON ; ty=ty ; EQ ; e1 = expr ; IN ; e2 = expr {SLet {is_rec = true; name = x;  args= concat y y1; ty=ty; value=e1; body=e2}}
  | FUN; x = arg ; x2 = arg*; ARROW; e = expr { SFun {arg = x; args = x2; body = e}}
  | e = expr2 { e }

expr2:
  | e1 = expr2; op = bop; e2 = expr2 { SBop (op, e1, e2) }
  | ASSERT ; e = expr3 { SAssert e}
  | e = expr3; es = expr3* { mk_app e es }
expr3:
  | UNIT { SUnit }
  | TRUE { STrue }
  | FALSE { SFalse }
  | n = NUM { SNum n }
  | x = VAR { SVar x }
  | LPAREN; e = expr; RPAREN { e }

%inline bop:
  | ADD { Add }
  | SUB { Sub }
  | MUL { Mul }
  | DIV { Div }
  | MOD { Mod }
  | LT { Lt }
  | LTE { Lte }
  | GT { Gt }
  | GTE { Gte }
  | EQ { Eq }
  | NEQ { Neq }
  | AND { And }
  | OR { Or }
