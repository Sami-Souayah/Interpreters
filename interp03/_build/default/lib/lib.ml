open Utils
include My_parser



let rec free_vars t =
  match t with
  | TInt | TBool | TFloat | TUnit -> []
  | TVar x -> [x]
  | TFun (t1, t2) -> (free_vars t1)@(free_vars t2)
  | TList t_elem | TOption t_elem -> free_vars t_elem
  | TPair (t1, t2) -> (free_vars t1)@(free_vars t2)

let ty_subst t x =
      let rec go = function
        | TInt -> TInt
        | TBool -> TBool
        | TFloat -> TFloat
        | TUnit -> TUnit
        | TVar y -> if x = y then t else TVar y
        | TFun (t1, t2) -> TFun (go t1, go t2)
        | TList t_elem -> TList (go t_elem )
        | TOption t_elem -> TOption (go t_elem)
        | TPair (t1,t2) -> TPair (go t1, go t2)
      in go



let ty_subst_c t x (t1, t2) = (ty_subst t x t1, ty_subst t x t2)
let ty_subst_cs t x = List.map (ty_subst_c t x)


let unify t (cst:constr list) : ty_scheme option =
  let rec go e = match e with
    | [] -> None
    | [TVar "$_out", t] -> Some (Forall ((free_vars t), t))
    | (t1, t2) :: cs when t1 = t2 -> go cs
    | (TFun (t1, t2), TFun (t1', t2')) :: cs ->
      go ((t1, t1') :: (t2, t2') :: cs)
    | (TVar x, t) :: cs ->
      if List.mem x (free_vars t)
      then None
      else go (ty_subst_cs t x cs)
    | (t, TVar x) :: cs -> go ((TVar x, t) :: cs)
    | (TOption t1, TOption t2)::cs -> go ((t1,t2)::cs)
    | (TPair (t1,t2),TPair (t1',t2'))::cs -> go((t1,t1')::(t2,t2')::cs)
    | (TList t1, TList t2)::cs -> go((t1,t2)::cs)
    | _ -> None
  in go (cst @ [TVar "$_out", t])

  
  
let type_of ctxt e =
        let rec type_of' (ctxt : stc_env) (e : expr) : ty * constr list =
          match e with
          | Unit -> (TUnit, [])
          | True | False -> (TBool, [])
          | Int _ -> (TInt, [])
          | Float _ -> (TFloat, [])
          | Nil -> (TList (TVar(gensym())),[])
          | ENone -> (TOption(TVar(gensym())),[])
          | ESome _ -> let t1,c1 = type_of' ctxt e in (TOption(t1), c1)
          | Var x ->
            let Forall (bnd_vars, t )= Env.find x ctxt in
            let rec instantiate bnd_vars t =
              match bnd_vars with
              | [] -> t
              | x :: bnd_vars ->
                let b = TVar (gensym ()) in
                instantiate bnd_vars (ty_subst b x t)
            in
            ( instantiate bnd_vars t
            , []
            )          
          | App (e1, e2) ->
              let t1, c1 = type_of' ctxt e1 in
              let t2, c2 = type_of' ctxt e2 in
              let ret_ty = TVar (gensym ()) in
              (ret_ty, (t1, TFun (t2, ret_ty)) :: c1 @ c2)
          | Annot (e, ty) ->
              let e_ty, constraints = type_of' ctxt e in
              (e_ty, (e_ty, ty) :: constraints)
          | Bop (op, e1, e2) ->
           ( match op with
              | Add -> let t1,c1 = type_of' ctxt e1 in 
              let t2,c2 = type_of' ctxt e2 in
                (TInt, (t1, TInt)::(t2,TInt)::c1@c2)
              | Sub  -> let t1,c1 = type_of' ctxt e1 in 
              let t2,c2 = type_of' ctxt e2 in
              (TInt, (t1, TInt)::(t2,TInt)::c1@c2)
              | Mul  -> let t1,c1 = type_of' ctxt e1 in 
              let t2,c2 = type_of' ctxt e2 in
              (TInt, (t1, TInt)::(t2,TInt)::c1@c2)
              | Div  -> let t1,c1 = type_of' ctxt e1 in 
              let t2,c2 = type_of' ctxt e2 in
              (TInt, (t1, TInt)::(t2,TInt)::c1@c2)
              | Mod -> let t1,c1 = type_of' ctxt e1 in 
              let t2,c2 = type_of' ctxt e2 in
              (TInt, (t1, TInt)::(t2,TInt)::c1@c2)
              | Eq -> let t1,c1 = type_of' ctxt e1 in 
              let t2,c2 = type_of' ctxt e2 in
              (TBool, (t1, t2)::c1@c2)
              | Neq  -> let t1,c1 = type_of' ctxt e1 in 
                let t2,c2 = type_of' ctxt e2 in
              (TBool, (t1, t2)::c1@c2)
              | Lt  -> let t1,c1 = type_of' ctxt e1 in 
              let t2,c2 = type_of' ctxt e2 in
              (TBool, (t1, t2)::c1@c2)
              | Lte  -> let t1,c1 = type_of' ctxt e1 in 
              let t2,c2 = type_of' ctxt e2 in
              (TBool, (t1, t2)::c1@c2)
              | Gt  -> let t1,c1 = type_of' ctxt e1 in 
              let t2,c2 = type_of' ctxt e2 in
              (TBool, (t1, t2)::c1@c2)
              | Gte -> let t1,c1 = type_of' ctxt e1 in 
              let t2,c2 = type_of' ctxt e2 in
              (TBool, (t1, t2)::c1@c2)
              | And  -> let t1,c1 = type_of' ctxt e1 in 
              let t2,c2 = type_of' ctxt e2 in
              (TBool, (t1, TBool)::(t2,TBool)::c1@c2)
              | Or -> let t1,c1 = type_of' ctxt e1 in 
              let t2,c2 = type_of' ctxt e2 in
              (TBool, (t1, TBool)::(t2,TBool)::c1@c2)
              | Cons -> let t1,c1 = type_of' ctxt e1 in 
              let t2,c2 = type_of' ctxt e2 in
              (TList t1, (t2, TList t1)::c1@c2)
              | Concat  -> let t1,c1 = type_of' ctxt e1 in 
              let t2,c2 = type_of' ctxt e2 in let fresh = TVar(gensym()) in
              (TList fresh, (t1, TList fresh)::(t2,TList fresh)::c1@c2)
              | Comma  -> let t1,c1 = type_of' ctxt e1 in 
              let t2,c2 = type_of' ctxt e2 in
              (TPair (t1,t2),c1@c2)
              | AddF -> let t1,c1 = type_of' ctxt e1 in 
              let t2,c2 = type_of' ctxt e2 in
              (TFloat, (t1, TFloat)::(t2,TFloat)::c1@c2)
              | SubF -> let t1,c1 = type_of' ctxt e1 in 
              let t2,c2 = type_of' ctxt e2 in
              (TFloat, (t1, TFloat)::(t2,TFloat)::c1@c2)
              | MulF -> let t1,c1 = type_of' ctxt e1 in 
              let t2,c2 = type_of' ctxt e2 in
              (TFloat, (t1, TFloat)::(t2,TFloat)::c1@c2)
              | DivF -> let t1,c1 = type_of' ctxt e1 in 
              let t2,c2 = type_of' ctxt e2 in
              (TFloat, (t1, TFloat)::(t2,TFloat)::c1@c2)
              | PowF -> let t1,c1 = type_of' ctxt e1 in 
              let t2,c2 = type_of' ctxt e2 in
              (TFloat, (t1, TFloat)::(t2,TFloat)::c1@c2))
          | If (e1, e2, e3) ->
              let t1, c1 = type_of' ctxt e1 in
              let t2, c2 = type_of' ctxt e2 in
              let t3, c3 = type_of' ctxt e3 in
              (t3, (t1, TBool)::(t2, t3) :: c1 @ c2 @ c3)
          | Let { is_rec; name; value; body } ->
            if is_rec then
              let a = TVar (gensym ()) in
              let a2 = TVar (gensym ()) in
              let env = Env.add name (Forall ([], TFun (a, a2))) ctxt in
              let value_ty, c1 = type_of' env value in
              let c_rec = [(value_ty, TFun (a, a2))] in
              let env2 = Env.add name (Forall ([], TFun (a, a2))) ctxt in
              let body_ty, c2 = type_of' env2 body in
              (body_ty, c_rec @ c1 @ c2)
            
            (*
              if is_rec then
                let a = TVar (gensym ()) in
                let a2 = TVar (gensym()) in
                let env = Env.add name (Forall ([], TFun (a,a2))) ctxt in
                let value_ty, c1 = type_of' env value in
                let env2 = Env.add name (Forall ( ([], value_ty))) ctxt in
                let body_ty, c2 = type_of' env2 body in
                (body_ty, (value_ty, TFun(a,a2)) :: c1 @ c2) *)
              else
                let value_ty, c1 = type_of' ctxt value in
                let env = Env.add name (Forall ( [], value_ty)) ctxt in
                let body_ty, c2 = type_of' env body in
                (body_ty, c1 @ c2)
          | Fun (str,ty,e) -> (match ty with
            | Some a -> let env = Env.add str (Forall ([],a)) ctxt 
              in let t1,c1 = type_of' env e 
              in (TFun (a,t1),c1)
            | None -> let fresh = TVar (gensym()) 
              in let env = Env.add str (Forall ([],fresh)) ctxt 
              in let t1,c1 =type_of' env e in (TFun (fresh,t1), c1)
              )
          | ListMatch { matched; hd_name; tl_name; cons_case; nil_case } ->
              let t1,c1 = type_of' ctxt matched in
              let fresh = TVar (gensym()) in
              let env = Env.add hd_name (Forall ([],fresh)) (Env.add tl_name (Forall ([],TList fresh)) ctxt) in
              let t2,c2 = type_of' env cons_case in
              let t3,c3 = type_of' ctxt nil_case in
              (t2,((t1, TList (fresh))::(t2,t3)::(c1@c2@c3)))
          | OptMatch { matched; some_name; some_case; none_case } ->
              let t1,c1 = type_of' ctxt matched in
              let fresh = TVar(gensym()) in
              let env = Env.add some_name (Forall ([],fresh)) ctxt in
              let t2,c2 = type_of' env some_case in
              let result3 = TVar (gensym()) in
              let t3,c3 = type_of' ctxt none_case in 
              (result3, (t1, TOption(fresh))::(t2, result3)::(t3, result3)::(c1 @ c2 @ c3))
              (* (t3,(t1, TOption(fresh))::(t2,t3)::(c1@c2@c3)) *)
          | PairMatch { matched; fst_name; snd_name; case } ->
              let t1,c1 = type_of' ctxt matched in
              let fresh1 = TVar (gensym()) in
              let fresh2 = TVar (gensym()) in
              let env = Env.add fst_name (Forall ([], fresh1)) (Env.add snd_name (Forall ([],fresh2)) ctxt) in
              let t2,c2 = type_of' env case in
              (t2,(t1,TPair(fresh1,fresh2))::(c1@c2))
          | _ -> failwith "Expression not implemented"
        in let ty,constraints = type_of' ctxt e in
        unify ty constraints

exception AssertFail
exception DivByZero
exception RecWithoutArg
exception CompareFunVals
let eval_expr (env : dyn_env) (e : expr) : value =
  let rec eval env e =
    match e with
    | Unit -> VUnit
    | True -> VBool true
    | False -> VBool false
    | Int n -> VInt n
    | Float f -> VFloat f
    | Nil -> VList []
    | ENone -> VNone
    | ESome e -> VSome (eval env e)
    | Var x -> Env.find x env
    | Bop (op, e1, e2) -> 
        (match op with
        | Add -> (match eval env e1, eval env e2 with
          | VInt n1, VInt n2 -> VInt (n1+n2)
          | _ -> failwith "No good" )
        | Sub -> (match eval env e1, eval env e2 with
          | VInt n1, VInt n2 -> VInt (n1-n2)
          | _ -> failwith "No good")
        | Mul -> (match eval env e1, eval env e2 with 
          | VInt n1, VInt n2 -> VInt (n1*n2)
          | _ -> failwith "No good")
        | Div -> (match eval env e1, eval env e2 with 
          | VInt n1, VInt n2 when n1<>0 -> VInt (n1/n2)
          | VInt _, VInt _ -> raise DivByZero
          | _ -> failwith "No good")
        | AddF -> (match eval env e1, eval env e2 with 
          | VFloat n1, VFloat n2 -> VFloat (n1+.n2)
          | _ -> failwith "No good")
        | SubF -> (match eval env e1, eval env e2 with 
          | VFloat n1, VFloat n2 -> VFloat (n1-.n2)
          | _ -> failwith "No good")
        | MulF -> (match eval env e1, eval env e2 with 
          | VFloat n1, VFloat n2 -> VFloat (n1*.n2)
          | _ -> failwith "No good")
        | DivF -> (match eval env e1, eval env e2 with 
          | VFloat n1, VFloat n2 when n1<>0.0-> VFloat (n1/.n2)
          | VFloat _, VFloat _ -> raise DivByZero
          | _ -> failwith "No good")
        | PowF -> (match eval env e1, eval env e2 with 
          | VFloat n1, VFloat n2 -> VFloat (n1**n2)
          | _ -> failwith "No good")
        | Mod -> (match eval env e1, eval env e2 with
          | VInt n1, VInt n2 -> VInt (n1 mod n2)
          | _ -> failwith "No good")
        | Lt -> (match eval env e1, eval env e2 with 
          | VClos _ , _ -> raise CompareFunVals
          | _ , VClos _ -> raise CompareFunVals
          | n1, n2 -> VBool (n1<n2))
        | Gt -> (match eval env e1, eval env e2 with 
          | VClos _ , _ -> raise CompareFunVals
          | _ , VClos _ -> raise CompareFunVals
          | n1, n2 -> VBool (n1>n2))
        | Lte -> (match eval env e1, eval env e2 with 
          | VClos _ , _ -> raise CompareFunVals
          | _ , VClos _ -> raise CompareFunVals
          | n1, n2 -> VBool (n1<=n2))
        | Gte -> (match eval env e1, eval env e2 with 
          | VClos _ , _ -> raise CompareFunVals
          | _ , VClos _ -> raise CompareFunVals
          | n1, n2 -> VBool (n1>=n2))
        | Eq -> (match eval env e1, eval env e2 with 
          | VClos _ , _ -> raise CompareFunVals
          | _ , VClos _ -> raise CompareFunVals
          | n1, n2 -> VBool (n1=n2))
        | Neq -> (match eval env e1, eval env e2 with 
          | VClos _ , _ -> raise CompareFunVals
          | _ , VClos _ -> raise CompareFunVals
          | n1, n2 -> VBool (n1<>n2))
        | Comma -> (match eval env e1, eval env e2 with 
          | n1, n2 -> VPair (n1,n2))
        | Concat -> (match eval env e1, eval env e2 with 
          | VList n1, VList n2 -> VList (n1@n2)
          | _ -> failwith "No good")
        | And -> (match eval env e1 with
          | VBool true -> eval env e2
          | VBool false -> VBool false
          | _ -> failwith "No good")
        | Or -> (match eval env e1 with
          | VBool true -> VBool true
          | VBool false -> eval env e2
          | _ -> failwith "No good")
        | Cons -> (match eval env e1, eval env e2 with 
          | poo, VList n2 -> VList (poo::n2)
          | _ -> failwith "No good"))
         
      
        (*make sure to short circuit (eval v1 before v2 on and and or)*)
        (*
        (match (op, v1, v2) with
        | (Add, VInt n1, VInt n2) -> VInt (n1 + n2)
        | (Sub, VInt n1, VInt n2) -> VInt (n1 - n2)
        | (Mul, VInt n1, VInt n2) -> VInt (n1 * n2)
        | (Div, VInt n1, VInt n2) -> if n2 = 0 then raise DivByZero else VInt (n1 / n2)
        | (AddF, VFloat f1, VFloat f2) -> VFloat (f1 +. f2)
        | (SubF, VFloat f1, VFloat f2) -> VFloat (f1 -. f2)
        | (MulF, VFloat f1, VFloat f2) -> VFloat (f1 *. f2)
        | (PowF, VFloat f1, VFloat f2) -> VFloat (f1**f2)
        | (DivF, VFloat f1, VFloat f2) -> if f2 = 0.0 then raise DivByZero else VFloat (f1 /. f2)
        | (Lt, VClos _,_) | (Lt, _,VClos _) | (Lte, VClos _, _) | (Lte, _,VClos _ ) | (Gt, VClos _, _) | (Gt, _,VClos _ ) | (Gte, VClos _, _) | (Gte, _,VClos _ ) | (Eq, VClos _, _) | (Eq, _,VClos _ ) | (Neq, VClos _, _) | (Neq, _,VClos _ ) -> raise CompareFunVals
        | (Gt, n1, n2) -> VBool (n1>n2)
        | (Lt, n1, n2) -> VBool (n1 < n2)
        | (Gte, n1, n2) -> VBool (n1>=n2)
        | (Lte, n1, n2) -> VBool (n1<=n2)
        | (Eq, v1, v2) -> VBool (v1 = v2)
        | (Neq, v1,v2) -> VBool (v1<>v2)
        | (And, VBool v1, VBool v2) -> VBool (v1&&v2)
        | (Or, VBool v1, VBool v2 ) -> VBool (v1||v2)
        | (Cons, v, VList l) -> VList (v :: l)
        | (Comma, v1,v2) -> VPair (v1,v2)
        | (Concat, VList n1, VList n2) -> VList (n1@n2)
        | _ -> failwith "Unsupported operation or mismatched types")
        *)
    | If (cond, e_then, e_else) -> (
        match eval env cond with
        | VBool true -> eval env e_then
        | VBool false -> eval env e_else
        | _ -> failwith "Condition must evaluate to a boolean")
    | ListMatch { matched; hd_name; tl_name; cons_case; nil_case } -> (
        match eval env matched with
        | VList [] -> eval env nil_case
        | VList (hd :: tl) ->
            let env' = Env.add hd_name hd (Env.add tl_name (VList tl) env) in
            eval env' cons_case
        | _ -> failwith "Expected a list")
    | OptMatch { matched; some_name; some_case; none_case } -> (
        match eval env matched with
        | VNone -> eval env none_case
        | VSome v ->
            let env' = Env.add some_name v env in
            eval env' some_case
        | _ -> failwith "Expected an option")
    | PairMatch { matched; fst_name; snd_name; case } -> (
        match eval env matched with
        | VPair (v1, v2) ->
            let env' = Env.add fst_name v1 (Env.add snd_name v2 env) in
            eval env' case
        | _ -> failwith "Expected a pair")
    | Fun (arg, _, body) -> VClos { name = None; arg; body; env }
    | App (e1, e2) -> 
        (match eval env e1 with
        | VClos {name=None;arg=arg2;body=body2;env=env2} -> 
          let arg_value = eval env e2 in
          let env' = Env.add arg2 arg_value env2 in
          eval env' body2
        | VClos {name=Some name3;arg=arg3;body=body3;env=env3} -> 
          let arg_val = eval env e2 in 
          let env' = Env.add name3 (VClos {name=Some name3;arg = arg3;body = body3;env=env3}) (Env.add arg3 arg_val env3) in
          eval env' body3
        | _ -> failwith "Not a function" )
      
    | Let { is_rec; name=bigname; value; body } ->
      let value_eval =
        if is_rec=true then
            (match eval env value with
              | VClos {name=stuff;arg;body;env} when stuff<>None -> VClos {name=Some bigname;arg;body;env}
              | _ -> raise RecWithoutArg)
        else eval env value
      in
      eval (Env.add bigname value_eval env) body
    | Annot (e, _) -> eval env e
    | Assert e -> (
      match eval env e with
      | VBool true -> VUnit
      | VBool false -> raise AssertFail
      | _ -> failwith "Assertion error")
  in
  eval env e

let type_check =
  let rec go ctxt = function
  | [] -> Some (Forall ([], TUnit))
  | {is_rec;name;value} :: ls ->
    match type_of ctxt (Let {is_rec;name;value; body = Var name}) with
    | Some ty -> (
      match ls with
      | [] -> Some ty
      | _ ->
        let ctxt = Env.add name ty ctxt in
        go ctxt ls
    )
    | None -> None
  in go Env.empty

let eval p =
  let rec nest = function
    | [] -> Unit
    | [{is_rec;name;value}] -> Let {is_rec;name;value;body = Var name}
    | {is_rec;name;value} :: ls -> Let {is_rec;name;value;body = nest ls}
  in eval_expr Env.empty (nest p)

let interp input =
  match parse input with
  | Some prog -> (
    match type_check prog with
    | Some ty -> Ok (eval prog, ty)
    | None -> Error TypeError
  )
  | None -> Error ParseError
