(* Semantic checking for easel compiler *)

open Ast

module StringMap = Map.Make(String)

(* The semantic checker will return void if successful and will throw an exception otherwise *)

let globals = Hashtbl.create 8;;


let check (functions, statements) =
  
  (* check for duplicates names *)
  let report_dup exceptf list =
      let rec helper = function
          n1 :: n2 :: _ when n1 = n2 -> raise (Failure (exceptf n1))
        | _ :: t -> helper t
        | [] -> ()
      in helper (List.sort compare list)
  in

  (* check for void type *)
  let check_void exceptf = function
        (Void, n) -> raise (Failure (exceptf n))
      | _ -> ()
  in

  (* check that rvalue type can be assigned to lvalue type *)
  let check_assign lvalt rvalt err =
      if lvalt = rvalt then lvalt
      else if (lvalt = Pix && rvalt = Int) then lvalt
      else raise err
  in

  (* Check and build function table *)
  let functions =
        { typ = Void; fname = "draw_default"; formals = [];
          body = []; checked = true }::
        { typ = Void; fname = "do_draw"; formals = [(Pix, DecArr(DecArr(DecId("canvas"), 0), 0))];
        body = []; checked = true }::
        { typ = Void; fname = "draw"; formals = [(Pix, DecArr(DecArr(DecId("canvas"), 0), 0))];
        body = []; checked = true }::
      { typ = Float; fname = "pow"; formals = [(Float, DecId("x")); (Float, DecId("y"))];
          body = []; checked = true }::
      { typ = Float; fname = "tan"; formals = [(Float, DecId("x"))];
          body = []; checked = true }::
      { typ = Float; fname = "sin"; formals = [(Float, DecId("x"))];
          body = []; checked = true }::
      { typ = Float; fname = "cos"; formals = [(Float, DecId("x"))];
          body = []; checked = true }::
      { typ = Float; fname = "log"; formals= [(Float, DecId("base")); (Float,  DecId("value"))];
          body = []; checked = true }::
      { typ = Float; fname = "rando"; formals = [];
          body = []; checked = true }:: 
      { typ = Float; fname = "randos"; formals = [(Int, DecId("seed"))];
          body = []; checked = true }:: functions

  in

    let rec typ_of_bind = function
          (t, DecId(_)) -> t
        | (t, DecArr(d, _)) -> typ_of_bind (Arr(t), d)
    in

    let func_sign fd =
        string_of_typ fd.typ ^ fd.fname ^
        List.fold_left (fun s fm -> s ^ string_of_typ (typ_of_bind fm)) "" fd.formals
    in

  report_dup (fun n -> "duplicate function " ^ n)
      (List.map (fun fd -> func_sign fd) functions);

  let func_decls = List.fold_left (fun m fd -> StringMap.add (func_sign fd) fd m)
    StringMap.empty functions
  in

  let func_decl s = try StringMap.find s func_decls
      with Not_found -> raise (Failure ("unrecognized function " ^ s))
  in

    (* a main function isn't required for easel *)
    (*let _ = func_decl "main" in*)
    let type_of_identifier locals id =
      try StringMap.find id locals
      with Not_found ->
          try Hashtbl.find globals id
          with Not_found -> raise (Failure ("undeclared identifier " ^ id))
    in

    let length_of_arr_identifier locals id =
      try StringMap.find id locals
      with Not_found ->
          try Hashtbl.find globals id
          with Not_found -> raise (Failure ("undeclared identifier " ^ id))
    in

    let rec id_of_dectr = function
          DecId(id) -> id
        | DecArr(d, _) -> id_of_dectr d
    in

    let rec id_of_lval e = match e with
          Id(id) -> id
        | EleAt(arr, _) -> id_of_lval arr
        | _ -> raise(Failure ("illegal left value " ^ string_of_expr e))
    in

    let dimension_of_array e =
      let rec helper dimension = function
        Id(id) -> dimension
      | EleAt(arr, length) -> helper (dimension + 1) arr
    in helper 0 e
	in
    
    let rec length_of_arrdectr = function
        | DecArr(DecId(_), l) -> [l] 
        | DecArr(DecArr(DecId(_),len1),len2)-> [len1;len2]
    in

    (* Return the type of an expression or throw an exception *)
    let rec expr locals = function
        IntLit _ -> Int
      | FloatLit _ -> Float
      | BoolLit _ -> Bool
      | PixLit(el)-> (*match el with [e1; e2; e3] -> *)
        (match el with
          [e1; e2; e3] -> let t1 = expr locals e1 and t2 = expr locals e2 and t3 = expr locals e3 in
                          if (t1 = Int && t2 = Int && t3 = Int) then Pix 
                          else raise(Failure ("illegal pix value [" ^ string_of_expr e1 ^ string_of_expr e2 ^ string_of_expr e3 ^ "]"))
        | _ -> raise(Failure("Incorrect amount of arguments to use a pix literal")))
      | ArrLit(el) as arrl -> let t = expr locals (List.hd el) in
                              let rec tm typ = (function
                                  [] -> Arr(typ)
                                | _ as l -> let h = List.hd l in
                                            if typ = (expr locals h) then tm typ (List.tl l)
                                            else raise(Failure ("Array types in array literal " ^ string_of_expr arrl ^ " do not match"))) in
                              tm t el
                           
      | Id s -> type_of_identifier locals s
      | Binop(e1, op, e2) as e -> let t1 = expr locals e1 and t2 = expr locals e2 in
        (match op with
            Add | Sub | Mult | Div when t1 = Int && t2 = Int -> Int
          | Pow when (t1 = Int || t1 = Float) && (t2 = Int || t2 = Float) -> Float
          | Equal | Neq when t1 = t2 -> Bool
          | Less | Leq | Greater | Geq when t1 = Int && t2 = Int -> Bool
          | And | Or when t1 = Bool && t2 = Bool -> Bool
          | _ -> raise (Failure ("illegal binary operator " ^
            string_of_typ t1 ^ " " ^ string_of_op op ^ " " ^
            string_of_typ t2 ^ " in " ^ string_of_expr e))
        )
      | Unop(op, e) as ex -> let t = expr locals e in
      (match op with
            Neg when t = Int -> Int
          | Not when t = Bool -> Bool
          (*TODO: check ++ and -- *)
          | Inc | Dec when t = Int -> Int
          (*TODO: check **, //, ^^ *)
          | UMult | UDiv when t = Int -> Int
          | UPow when t = Int or t = Float -> Float 
          | _ -> raise (Failure ("illegal unary operator " ^ string_of_uop op ^
           string_of_typ t ^ " in " ^ string_of_expr ex))
        )
      | Noexpr -> Void
      | Assign(var, e) as ex -> 
          let lt = type_of_identifier locals (id_of_lval var)
            and rt = expr locals e in
            (* TODO: check that var's dimension is <= lt *)
           (* match var with
            EleAt(Id(a), length) -> 
            let arrlen_in_map = fst (length_of_arrdectr (snd (StringMap.find locals a))) in 
              if length < arrlen_in_map then check_assign lt rt (Failure ("illegal assignment " ^ 
                  string_of_typ lt ^ " = " ^ string_of_typ rt ^ " in " ^ string_of_expr ex))
              else raise(Failure ("illegal dimension access " ^ string_of_expr el))
          | EleAt(EleAt(Id(a),length1),length2) -> 
            let arrlen1_in_map = fst (length_of_arrdectr (snd (StringMap.find locals a)))
              and arrlen2_in_map = snd (length_of_arrdectr (snd (StringMap.find locals a))) in
              if length1 < arrlen1_in_map && length2 < arrlen2_in_map then check_assign lt rt (Failure ("illegal assignment " ^ 
                  string_of_typ lt ^ " = " ^ string_of_typ rt ^ " in " ^ string_of_expr ex))
              else raise(Failure ("illegal dimension access " ^ string_of_expr el))
          | _ ->*) check_assign lt rt (Failure ("illegal assignment " ^ 
                  string_of_typ lt ^ " = " ^ string_of_typ rt ^ " in " ^ string_of_expr ex))


      | Call(fdectr, actuals) as call ->
         (* TODO: find out the correct signature of fd according to the calling context
          * and then use it to fetch fd *)
         let fd = snd (StringMap.choose func_decls) in
         if List.length actuals != List.length fd.formals then
           raise (Failure ("expecting " ^ string_of_int
             (List.length fd.formals) ^ " arguments in " ^ string_of_expr call))
         else
           List.iter2 (fun (ft, _) e -> let et = expr locals e in
              ignore (check_assign ft et
                (Failure ("illegal actual argument found " ^ string_of_typ et ^
                " expected " ^ string_of_typ ft ^ " in " ^ string_of_expr e))))
             fd.formals actuals;
           if not fd.checked then check_func fd else ();
           fd.typ
      | EleAt(arr, _) as ele-> (match arr with
                           EleAt(iarr, _) -> let iat = expr locals iarr in
                                             (match iat with
                                               Arr(Arr(arr_t)) -> arr_t
                                             | _ -> raise(Failure (string_of_expr ele ^ " is not a valid array")))
                         | _ -> let iat = expr locals arr in
                                             (match iat with
                                               Arr(Arr(arr_t)) -> Arr(arr_t)
                                             | Arr(arr_t) -> arr_t
                                             | _ -> raise(Failure (string_of_expr ele ^ " is not a valid array"))))
                           
      | PropAcc(e, prp) -> 
            (* Find the type of a given thing *)
            let t = expr locals e in
            (* Make sure the property works for the type *)
            (match t with 
              Pix -> (match prp with
                             "red" | "green" | "blue" -> Int
                            | _ -> raise(Failure ("invalid pixel property " ^ prp))) 
            | Arr(_) -> (match prp with
                             "size" -> Int
                            | _ -> raise(Failure ("invalid array property " ^ prp)))
            | _ -> raise(Failure ("type " ^ string_of_typ t ^ "has no valid property " ^ prp)))

      (*TODO: check anonymous function*)
      | AnonFunc(fdecl) -> Func(Void, [])
      | _ as unk -> raise(Failure (string_of_expr unk ^ " is an unknown expression type"))

    and check_func func =
        (* TODO: check formals for being void and duplicate *)
        (* TODO: replace this empty map by a map of formals *)
        check_stmt StringMap.empty func.typ (Block func.body)

    and check_vdef l t = function
        InitDectr(d, Noexpr) -> typ_of_bind (t, d)
      | InitDectr(d, e) as initd ->
        let lt = typ_of_bind (t, d)
        and rt = expr l e in
        check_assign lt rt (Failure ("illegal initialization " ^ string_of_typ lt ^
        " = " ^ string_of_typ rt ^ " in " ^ string_of_typ t ^ string_of_initdectr initd))

    and add_locals locals t initds =
        List.fold_left (fun m initd -> match initd with InitDectr(d, e) ->
            let tt = check_vdef locals t initd in
            let id = id_of_dectr d in
            if not (StringMap.mem id locals) then StringMap.add id (*fst initd*)tt locals
            else raise (Failure ("duplicate local " ^ id))
        ) locals initds


    and check_block locals funct = function
        [Return _ as s] -> check_stmt locals funct s
      | Return _ :: _ -> raise (Failure "nothing may follow a return")
      | Block sl :: ss -> check_block locals funct sl; check_block locals funct ss
      | Vdef(t, initds) :: ss -> check_block (add_locals locals t initds) funct ss
      | s :: ss -> check_stmt locals funct s; check_block locals funct ss
      | [] -> ()

    and check_bool_expr l e = if expr l e != Bool
        then raise (Failure ("expected Boolean expression in " ^ string_of_expr e))
        else ()

    and check_stmt locals funct = function
        Block sl -> check_block locals funct sl
      | Expr e -> ignore (expr locals e)
      | Return e -> let t = expr locals e in if t = funct then () else
         raise (Failure ("return gives " ^ string_of_typ t ^ " expected " ^
                         string_of_typ funct ^ " in " ^ string_of_expr e))
      | If(p, b1, b2) -> check_bool_expr locals p; check_stmt locals funct b1; check_stmt locals funct b2
      | For(e1, e2, e3, st) -> ignore (expr locals e1); check_bool_expr locals e2;
                               ignore (expr locals e3); check_stmt locals funct st
      | While(p, s) -> check_bool_expr locals p; check_stmt locals funct s
      | Vdef(t, ids) -> raise (Failure ("declaring local variable is only allowed in blocks"))
    in

    (*Only variables defined outside any block are globals*)
    let check_global_stmt = function
        (* initds: init_dectr list *)
        Vdef(t, initds) -> List.iter
            (fun initd -> match initd with InitDectr(d, e) ->
                let tt = check_vdef StringMap.empty t initd in
                let id = id_of_dectr d in
                if not (Hashtbl.mem globals id) then Hashtbl.add globals id (*fst initd*)tt
                else raise (Failure ("duplicate global " ^ id)))
            initds
      | stmt -> check_stmt StringMap.empty Int stmt
    in

    (*List.iter check_func functions*)
    List.iter check_global_stmt (List.rev statements)
    (* TODO: Check for remaining functions not called by anyone?*)

