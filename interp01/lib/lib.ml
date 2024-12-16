open Utils
let parse = My_parser.parse

let expr_of_val v =
    match v with
     | VNum x -> Num x
     | VBool x -> if x = true then True else False
     | VUnit -> Unit
     | VFun (x,y) -> Fun (x,y)

let rec replace_var x y e =
   match e with
      | Num _ -> e
      | True -> e 
      | False -> e 
      | Unit -> e
      | Var z -> if z = y then Var x else Var z
      | App (e1, e2) -> App (replace_var x y e1, replace_var x y e2)
      | Fun (z, e) -> Fun (z, replace_var x y e)
      | Let (z, e1, e2) -> Let(z, replace_var x y e1, replace_var x y e2)
      | If (e1, e2, e3) -> If(replace_var x y e1, replace_var x y e2, replace_var x y e3)
      | Bop (op, e1, e2) -> Bop(op, replace_var x y e1, replace_var x y e2)

let rec subst v x e=
        match e with
          | Num _ -> e
          | True -> e
          | False -> e
          | Unit -> e
          | Bop (op,e1,e2) -> Bop (op, subst v x e1, subst v x e2)
          | Var y -> if x = y then expr_of_val v else Var y
          | App (e1, e2) -> App (subst v x e1, subst v x e2)
          | Fun (y, e) ->
            if x = y
            then Fun (y, e)
            else
              let z = gensym () in
              Fun (z, subst v x (replace_var z y e))
          | Let (y, e1, e2) ->
            if x = y
            then Let (y, subst v x e1, e2)
            else
              let z = gensym () in
              Let (z, subst v x e1, subst v x (replace_var z y e2))
          | If (e1,e2,e3) -> If (subst v x e1, subst v x e2, subst v x e3)


let rec eval e =
  match e with
    | Var x -> Error (UnknownVar x)
    | Num x -> Ok (VNum x)
    | Unit -> Ok (VUnit)
    | True -> Ok (VBool true)
    | False -> Ok (VBool false)
    | Bop (op, e1, e2) ->(match op with
        | Div -> (match (eval e1, eval e2) with
            | Ok (VNum _),Ok (VNum 0) -> Error (DivByZero)
            | Ok (VNum x), Ok (VNum y) -> Ok (VNum (x/y))
            | Ok (_),Ok(_) -> Error (InvalidArgs Div)
            | Ok (_), Error(y) -> Error y
            | Error (x), Ok(_) -> Error x
            | Error x, Error _ -> Error x )
        | Mul -> (match (eval e1, eval e2) with 
            | Ok (VNum x),Ok (VNum y) -> Ok(VNum (x*y))
            | Ok (_),Ok(_) -> Error (InvalidArgs Mul)
            | Ok (_), Error(y) -> Error y
            | Error (x), Ok(_) -> Error x
            | Error x, Error _ -> Error x)
        | Add  -> (match (eval e1,eval e2) with 
            | Ok (VNum x),Ok (VNum y) -> Ok(VNum (x+y))
            | Ok (_),Ok(_) -> Error (InvalidArgs Add)
            | Ok (_), Error(y) -> Error y
            | Error (x), Ok(_) -> Error x
            | Error x, Error _ -> Error x)
        | Sub  -> (match (eval e1,eval e2) with 
            | Ok (VNum x),Ok (VNum y) -> Ok(VNum (x-y))
            | Ok (_),Ok(_) -> Error (InvalidArgs Sub)
            | Ok (_), Error(y) -> Error y
            | Error (x), Ok(_) -> Error x
            | Error x, Error _ -> Error x)
        | Mod  -> (match (eval e1,eval e2) with 
            | Ok (VNum x),Ok (VNum y) -> Ok(VNum (x mod y))
            | Ok (_),Ok(_) -> Error (InvalidArgs Mod)
            | Ok (_), Error(y) -> Error y
            | Error (x), Ok(_) -> Error x
            | Error x, Error _ -> Error x)
        | Lt  -> (match (eval e1,eval e2) with 
            | Ok (VNum x),Ok (VNum y) -> Ok(VBool (x<y))
            | Ok (_),Ok(_) -> Error (InvalidArgs Lt)
            | Ok (_), Error(y) -> Error y
            | Error (x), Ok(_) -> Error x
            | Error x, Error _ -> Error x)
        | Gt  -> (match (eval e1,eval e2) with 
            | Ok (VNum x),Ok (VNum y) -> Ok(VBool (x>y))
            | Ok (_),Ok(_) -> Error (InvalidArgs Gt)
            | Ok (_), Error(y) -> Error y
            | Error (x), Ok(_) -> Error x
            | Error x, Error _ -> Error x)
        | Lte  -> (match (eval e1,eval e2) with 
            | Ok (VNum x),Ok (VNum y) -> Ok(VBool (x<=y))
            | Ok (_),Ok(_) -> Error (InvalidArgs Lte)
            | Ok (_), Error(y) -> Error y
            | Error (x), Ok(_) -> Error x
            | Error x, Error _ -> Error x)
        | Gte  -> (match (eval e1,eval e2) with 
            | Ok (VNum x),Ok (VNum y) -> Ok(VBool (x>=y))
            | Ok (_),Ok(_) -> Error (InvalidArgs Gte)
            | Ok (_), Error(y) -> Error y
            | Error (x), Ok(_) -> Error x
            | Error x, Error _ -> Error x)
        | Eq  -> (match (eval e1,eval e2) with 
            | Ok (VNum x),Ok (VNum y) -> Ok(VBool (x=y))
            | Ok (_),Ok(_) -> Error (InvalidArgs Eq)
            | Ok (_), Error(y) -> Error y
            | Error (x), Ok(_) -> Error x
            | Error x, Error _ -> Error x)
        | Neq  -> (match (eval e1,eval e2) with 
            | Ok (VNum x),Ok (VNum y) -> Ok(VBool (x<>y))
            | Ok (_),Ok(_) -> Error (InvalidArgs Neq)
            | Ok (_), Error(y) -> Error y
            | Error (x), Ok(_) -> Error x
            | Error x, Error _ -> Error x)
        | And  -> (match (eval e1,eval e2) with 
            | Ok (VBool x),Ok (VBool y) -> Ok(VBool (x&&y))
            | Ok (_),Ok(_) -> Error (InvalidArgs And)
            | Ok (_), Error(y) -> Error y
            | Error (x), Ok(_) -> Error x
            | Error x, Error _ -> Error x)
        | Or  -> (match (eval e1,eval e2) with 
            | Ok (VBool x),Ok (VBool y) -> Ok(VBool (x||y))
            | Ok (_),Ok(_) -> Error (InvalidArgs Or)
            | Ok (_), Error(y) -> Error y
            | Error (x), Ok(_) -> Error x
            | Error x, Error _ -> Error x))
    | If (e1,e2,e3) -> (match eval e1 with
        | Ok (VBool x) -> if x=true then eval e2 else eval e3
        | Error ( x )-> Error (x)
        | Ok (_) -> Error (InvalidIfCond))
    | Fun (x, e) -> Ok (VFun (x, e))
    | App (e1, e2) ->   (match (eval e1, eval e2) with
        | Ok (VFun (x, e)) , Ok v2 -> eval(subst v2 x e)
        | Ok _ , Ok _ -> Error (InvalidApp)  
        | Error x, _ | _, Error x -> Error x )
    | Let (x, e1, e2) -> (match eval e1 with 
        | Ok (v) -> eval (subst v x e2)
        | Error (x) -> Error (x))


let interp input =
    match parse input with
    | Some expr -> eval expr
    | None -> Error (ParseFail)