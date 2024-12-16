open Utils

exception AssertFail
exception DivByZero

let parse = My_parser.parse

let rec desugar1 e =    
  match e with
  | SUnit -> Unit
  | STrue -> True
  | SFalse -> False
  | SNum n -> Num n
  | SVar x -> Var x
  | SApp (f,arg) -> App (desugar1 f, desugar1 arg)
  | SIf (cond, e1, e2) -> If (desugar1 cond, desugar1 e1, desugar1 e2)
  | SBop (op, e1, e2) -> Bop (op, desugar1 e1, desugar1 e2)
  | SAssert e -> Assert (desugar1 e)
  | SFun { arg = (x, ty); args ; body  } ->
    List.fold_right
      (fun (x, ty) acc -> Fun (x, ty, acc))
      ((x, ty) :: args)
      (desugar1 body)
  | SLet { is_rec; name; args; ty; value; body } ->
      let desugarval = List.fold_right (fun (x,ty) acc -> Fun (x,ty,acc)) args (desugar1 value) in let desugarval2 = List.fold_right (fun(_,ty) acc -> FunTy(ty,acc)) args ty 
  in Let {is_rec;name;ty=desugarval2;value=desugarval;body = desugar1 body}

  
let desugar (prog : prog) =
  desugar1 (List.fold_right (fun{is_rec;name;ty;args;value} acc -> SLet {is_rec;name;ty;args;value;body=acc}) prog SUnit)



let rec finder gam x =
  match gam with
  | [] -> None
  | (x',ty) :: rest -> if x=x' then Some ty else finder rest x

  let rec type_of1 gam e =
  match e with
  | Unit -> Ok UnitTy
  | True -> Ok BoolTy
  | False -> Ok BoolTy
  | Num _ -> Ok IntTy
  | Var x -> (match finder gam x with
    | Some ty -> Ok ty
    | None -> Error (UnknownVar x))
  | Bop (op,e1,e2) -> 
    let check_op gam e1 e2 expected_ty result_ty op =
      match (type_of1 gam e1, type_of1 gam e2) with
          | Ok ty1, Ok ty2 when ty1 = expected_ty && ty2 = expected_ty -> Ok result_ty
          | Ok ty1, Ok _ when ty1 <> expected_ty -> Error (OpTyErrL (op, expected_ty, ty1))
          | Ok _, Ok ty2 -> Error (OpTyErrR (op, expected_ty, ty2))
          | Error err, _ -> Error err
          | _, Error err -> Error err
    in
    (match op with
    | Add -> check_op gam e1 e2 IntTy IntTy op
    | Sub -> check_op gam e1 e2 IntTy IntTy op
    | Mul -> check_op gam e1 e2 IntTy IntTy op
    | Div -> check_op gam e1 e2 IntTy IntTy op
    | Mod -> check_op gam e1 e2 IntTy IntTy op
    | Lt -> check_op gam e1 e2 IntTy BoolTy op
    | Lte -> check_op gam e1 e2 IntTy BoolTy op
    | Gt -> check_op gam e1 e2 IntTy BoolTy op
    | Gte -> check_op gam e1 e2 IntTy BoolTy op
    | Eq -> check_op gam e1 e2 IntTy BoolTy op
    | Neq -> check_op gam e1 e2 IntTy BoolTy op
    | And -> check_op gam e1 e2 BoolTy BoolTy op
    | Or -> check_op gam e1 e2 BoolTy BoolTy op)
  | If (e1,e2,e3) -> (match type_of1 gam e1 with
    | Ok BoolTy -> (match type_of1 gam e2,type_of1 gam e3 with
      | Ok e1, Ok e2 when e1=e2 -> Ok e1
      | Ok e1, Ok e2 -> Error (IfTyErr (e1,e2))
      | Error err, _ -> Error err
      | _,Error err -> Error err)
    | Ok ty -> Error (IfCondTyErr ty)
    | Error err -> Error err)
  | App (e1, e2) -> (match type_of1 gam e1 with
    | Ok (FunTy (arg,ret))-> (match type_of1 gam e2 with
      | Ok ty when ty = arg -> Ok ret
      | Ok ty -> Error (FunArgTyErr (arg, ty))
      | Error err -> Error err)
    | Ok ty -> Error (FunAppTyErr ty)
    | Error err -> Error err)
  | Assert e -> (match type_of1 gam e with
    | Ok BoolTy -> Ok UnitTy
    | Ok ty -> Error (AssertTyErr ty)
    | Error err -> Error err)
  | Fun (arg, arg2, body) -> (match type_of1 ((arg,arg2)::gam) body with
    | Ok body1 -> Ok (FunTy (arg2, body1))
    | Error err -> Error err)
  | Let {is_rec ; name ; ty ; value;body} -> 
    if is_rec then (match type_of1 ((name, ty) :: gam) value with
      | Ok val2 when val2 = ty -> type_of1 ((name, ty) :: gam) body
      | Ok val2 -> Error (LetTyErr (ty, val2))
      | Error err -> Error err) else
    (match type_of1 gam value with
      | Ok val2 when val2 = ty -> type_of1 ((name,ty) :: gam) body
      | Ok val2 -> Error (LetTyErr (ty,val2))
      | Error err -> Error err)




let type_of e = 
  type_of1 [] e

let rec eval1 env e = 
  match e with
  | Unit -> VUnit
  | True -> VBool true
  | False -> VBool false
  | Num n ->  VNum n
  | Var x -> (
      match Env.find_opt x env with
        | Some v -> v
        | None -> failwith "Unknown variable"
    )
  | If (cond, e1, e2) -> (
      match eval1 env cond with
      | VBool true -> eval1 env e1
      | VBool false -> eval1 env e2
      | _ -> failwith "Must be a boolean"
    )
  | Bop (op, e1, e2) -> (match op with
    | Add -> (match eval1 env e1, eval1 env e2 with
      | VNum x,VNum y -> VNum(x+y)
      | _,_ -> failwith "Incorrect type")
    | Sub -> (match eval1 env e1,eval1 env e2 with
      | VNum x, VNum y -> VNum(x-y)
      | _,_ -> failwith "Incorrect type")
    | Mul -> (match eval1 env e1, eval1 env e2 with
      | VNum x, VNum y -> VNum(x*y)
      | _,_ -> failwith "Incorrect type")
    | Div -> (match eval1 env e1, eval1 env e2 with
      | VNum x, VNum y when y<>0 -> VNum (x/y)
      | VNum _, VNum y when y=0 -> raise DivByZero
      | _,_ -> failwith "Incorrect type")
    | Mod -> (match eval1 env e1, eval1 env e2 with
      | VNum x, VNum y -> VNum (x mod y)
      | _,_ -> failwith "Incorrect type")
    | Lt -> (match eval1 env e1, eval1 env e2 with
      | VNum x, VNum y -> VBool (x<y)
      | _,_ -> failwith "Incorrect type")
    | Gt -> (match eval1 env e1, eval1 env e2 with
      | VNum x, VNum y -> VBool (x>y)
      | _,_ -> failwith "Incorrect type")
    | Lte -> (match eval1 env e1, eval1 env e2 with
      | VNum x, VNum y -> VBool (x<=y)
      | _,_ -> failwith "Incorrect type")
    | Gte -> (match eval1 env e1, eval1 env e2 with
      | VNum x, VNum y -> VBool (x>=y)
      | _,_ -> failwith "Incorrect type")
    | Eq -> (match eval1 env e1, eval1 env e2 with
      | VNum x, VNum y -> VBool (x=y)
      | _,_ -> failwith "Incorrect type") 
    | Neq -> (match eval1 env e1, eval1 env e2 with
      | VNum x, VNum y -> VBool (x<>y)
      | _,_ -> failwith "Incorrect type")
    | And -> (match eval1 env e1, eval1 env e2 with
      | VBool x, VBool y -> VBool (x && y)
      | _,_ -> failwith "Incorrect type")
    | Or -> (match eval1 env e1, eval1 env e2 with
      | VBool x, VBool y -> VBool (x || y)
      | _,_ -> failwith "Incorrect type")
    )
  | Assert e -> (
      match eval1 env e with
      | VBool true -> VUnit
      | VBool false -> raise AssertFail
      | _ -> failwith "Assertion error"
    )
    | Fun (arg, _, body) -> VClos { name = None; arg=arg; body=body; env=env }
    | App (e1, e2) -> (
        (match eval1 env e1 with
        | VClos {name=name2;arg=arg2;body=body2;env=env2} when name2=None-> 
            (match eval1 env e2 with
            | arg_value -> 
                let extended_env = Env.add arg2 arg_value env2 in
                eval1 extended_env body2)
        | VClos {name=Some name3;arg=arg3;body=body3;env=env3} -> (match eval1 env e2 with
          | arg_val2 -> let extended2 = Env.add arg3 arg_val2 env3 in let extended3 = Env.add name3 (VClos {name=Some name3;arg=arg3;body=body3;env=env3}) extended2 in eval1 extended3 body3) 
        | _ -> failwith "App error")
      )
    | Let { is_rec; name=bigname; ty = _; value; body } ->
        let value_eval =
          if is_rec=true then
              (match eval1 env value with
                | VClos {name=_;arg;body;env} -> VClos {name=Some bigname;arg;body;env}
                | _ -> failwith "Not recursive")
          else eval1 env value
        in
        eval1 (Env.add bigname value_eval env) body

let eval e =
  eval1 Env.empty e

  let interp input =
    match parse input with
    | Some prog -> (
        match type_of (desugar prog) with
        | Ok _ -> Ok (eval (desugar prog)) 
        | Error err -> Error err
      )
    | None -> Error ParseErr