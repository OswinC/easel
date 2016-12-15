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
      else if ((lvalt = Pix && rvalt = Int) || (lvalt = ArrRef(Pix) && rvalt = ArrRef(Int)) || (lvalt = ArrRef(ArrRef(Pix)) && rvalt = ArrRef(ArrRef(Int)))) then lvalt
      else raise err
  in

  (* Check and build function table *)
  let functions =
        { typ = Void; fname = "draw_default"; formals = [];
          body = []; checked = true }::
        { typ = Void; fname = "do_draw"; formals = [(Pix, DecArr(DecArr(DecId("canvas"), 0), 0)); (Int, DecId("w")); (Int, DecId("h")); (Int, DecId("x")); (Int, DecId("y"))];
        body = []; checked = true }::
        { typ = Void; fname = "draw"; formals = [(Pix, DecArr(DecArr(DecId("canvas"), 0), 0)); (Int, DecId("w")); (Int, DecId("h"))];
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
        | (t, DecArr(d, _)) -> typ_of_bind (ArrRef(t), d)
    in

    let func_sign fd =
        fd.fname ^
        List.fold_left (fun s fm -> s ^ string_of_typ (typ_of_bind fm)) "" fd.formals
    in

  report_dup (fun n -> "duplicate function " ^ n)
      (List.map (fun fd -> func_sign fd) functions);

  let func_decls =  List.fold_left (fun m fd -> let formal_types = 
                        List.map (fun formals -> fst formals) fd.formals in 
                        ignore(Hashtbl.add globals fd.fname (Func(fd.typ, formal_types))); 
                        StringMap.add (func_sign fd) fd m)
    StringMap.empty functions
  in
 
  let func_decl func_locals s = 
            try StringMap.find s func_locals
      with Not_found ->
          try StringMap.find s func_decls
          with Not_found -> raise (Failure ("unrecognized function " ^ s))
  in

    (* a main function isn't required for easel *)
    (*let _ = func_decl "main" in*)
    let type_of_identifier locals id =
      (*ignore(print_endline("Current locals:"));ignore(StringMap.iter (fun f _ -> print_endline f) locals);
      ignore(print_endline("Current globals:"));ignore(Hashtbl.iter (fun f _ -> print_endline f) globals);*)
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
    let rec expr locals func_locals = function
        IntLit _ -> Int
      | FloatLit _ -> Float
      | BoolLit _ -> Bool
      | PixLit(el)-> (*match el with [e1; e2; e3] -> *)
        (match el with
          [e1; e2; e3] -> let t1 = expr locals func_locals e1 and t2 = expr locals func_locals e2 and t3 = expr locals func_locals e3 in
                          if (t1 = Int && t2 = Int && t3 = Int) then Pix 
                          else raise(Failure ("illegal pix value [" ^ string_of_expr e1 ^ string_of_expr e2 ^ string_of_expr e3 ^ "]"))
        | _ -> raise(Failure("Incorrect amount of arguments to use a pix literal")))
      | ArrLit(el) as arrl ->raise(Failure("Array literals are not currently supported")) (*let t = expr locals func_locals (List.hd el) in
                              let rec tm typ = (function
                                  [] -> ArrRef(typ)
                                | _ as l -> let h = List.hd l in
                                            if typ = (expr locals func_locals h) then tm typ (List.tl l)
                                            else raise(Failure ("Array types in array literal " ^ string_of_expr arrl ^ " do not match"))) in
                              tm t el*)
                           
      | Id s -> type_of_identifier locals s
      | Binop(e1, op, e2) as e -> let t1 = expr locals func_locals e1 and t2 = expr locals func_locals e2 in
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
      | Unop(op, e) as ex -> let t = expr locals func_locals e in
      (match op with
            Neg -> (match t with 
                        Int -> Int
                      | Float -> Float)
          | Not when t = Bool -> Bool
          | Inc | Dec -> (match t with 
                        Int -> Int
                      | Float -> Float)
          | _ -> raise (Failure ("illegal unary operator " ^ string_of_uop op ^
           string_of_typ t ^ " in " ^ string_of_expr ex))
        )
      | Noexpr -> Void
      | Assign(var, e) as ex -> 
            let lt = expr locals func_locals var
            and rt = expr locals func_locals e in
                  check_assign lt rt (Failure ("illegal assignment " ^ 
                  string_of_typ lt ^ " = " ^ string_of_typ rt ^ " in " ^ string_of_expr ex))


      | Call(fdectr, actuals) as call ->
         (match fdectr with
           Id fname -> let fsign = fname ^
                                   List.fold_left (fun s fm -> s ^ string_of_typ (expr locals func_locals fm)) "" actuals in
                      

         let fd = func_decl func_locals fsign in
         if List.length actuals != List.length fd.formals then
           raise (Failure ("expecting " ^ string_of_int
             (List.length fd.formals) ^ " arguments in " ^ string_of_expr call))
         else
           List.iter2 (fun b e -> let bt = typ_of_bind b in let et = expr locals func_locals e in
              ignore (check_assign bt et
                (Failure ("illegal actual argument found " ^ string_of_typ et ^
                " expected " ^ string_of_typ bt ^ " in " ^ string_of_expr e))))
             fd.formals actuals;
           if not fd.checked then check_func fd else ();
           fd.typ
          | _ -> raise(Failure(string_of_expr fdectr ^ " is not a valid function to call" )))
      | EleAt(arr, _) as ele-> (match arr with
                           EleAt(iarr, _) -> let iat = expr locals func_locals iarr in
                                             (match iat with
                                               ArrRef(ArrRef(arr_t)) -> arr_t
                                             | _ -> raise(Failure (string_of_expr ele ^ " is not a valid array")))
                         | _ -> let iat = expr locals func_locals arr in
                                             (match iat with
                                               ArrRef(ArrRef(arr_t)) -> ArrRef(arr_t)
                                             | ArrRef(arr_t) -> arr_t
                                             | _ -> raise(Failure (string_of_expr ele ^ " is not a valid array"))))
                           
      | PropAcc(e, prp) -> 
            (* Find the type of a given thing *)
            let t = expr locals func_locals e in
            (* Make sure the property works for the type *)
            (match t with 
              Pix -> (match prp with
                             "red" | "green" | "blue" -> Int
                            | _ -> raise(Failure ("invalid pixel property " ^ prp))) 
            | ArrRef(_) -> (match prp with
                             "size" -> Int
                            | _ -> raise(Failure ("invalid array property " ^ prp)))
            | _ -> raise(Failure ("type " ^ string_of_typ t ^ "has no valid property " ^ prp)))

      | AnonFunc(func_decl) -> let formal_types = List.map (fun (ftyp, _) -> ftyp) func_decl.formals  in
                                            Func(func_decl.typ, formal_types)

    and check_func func =
        report_dup (fun n -> "Duplicate formals in function " ^ func.fname) func.formals;
        List.iter (check_void (fun n -> "Formal arguments cannot have a void type" ^ string_of_dectr n)) func.formals;
        let func_formals = List.fold_left (fun m (typ, dect) -> (match typ with
                                                                 Func (t,f) -> let form_func_sign = (string_of_dectr dect) ^ 
                                                                               List.fold_left(fun s fm -> s ^ string_of_typ fm) "" f in
                                                                               let form_form_bind = List.map (fun fo -> (fo, DecId("novar"))) f in
                                                                               let fd = {typ = t; fname = string_of_dectr dect; formals = form_form_bind;
                                                                                         body=[]; checked=true} in
                                                                               StringMap.add form_func_sign fd m
                                                               | _ -> m)) StringMap.empty func.formals in
        let formals = List.fold_left (fun m (typ, dect) -> StringMap.add (string_of_dectr dect) typ m) StringMap.empty func.formals in
        (*ignore (StringMap.iter (fun f _ -> print_endline("Local formals: " ^ f)) formals);*)

        check_stmt formals func_formals func.typ (Block func.body);ignore(func.checked = true)

    and check_vdef l fl t = function
        InitDectr(d, Noexpr) -> typ_of_bind (t, d)
      | InitDectr(d, e) as initd ->
        let lt = typ_of_bind (t, d)
        and rt = expr l fl e in
        check_assign lt rt (Failure ("illegal initialization " ^ string_of_typ lt ^
        " = " ^ string_of_typ rt ^ " in " ^ string_of_typ t ^ string_of_initdectr initd))

    and add_locals locals func_locals t initds =
        (*ignore(print_endline("Init Dectrs: "));ignore(List.iter (fun i -> match i with InitDectr(d,e)-> print_endline(string_of_dectr d)) initds);*)
        List.fold_left (fun m initd -> match initd with InitDectr(d, e) ->
            let tt = check_vdef m func_locals t initd in
            let id = id_of_dectr d in (*ignore(print_endline("ID: " ^ id));*)
            if not (StringMap.mem id m) then StringMap.add id (*fst initd*)tt m 
            else raise (Failure ("duplicate local " ^ id))
        ) locals initds


    and check_block locals func_locals funct = function
        [Return _ as s] -> check_stmt locals func_locals funct s
      | Return _ :: _ -> raise (Failure "nothing may follow a return")
      | Block sl :: ss -> check_block locals func_locals funct sl; check_block locals func_locals funct ss
      | Vdef(t, initds) :: ss -> check_block (add_locals locals func_locals t (List.rev initds)) func_locals funct ss
      | s :: ss -> check_stmt locals func_locals funct s; check_block locals func_locals funct ss
      | [] -> ()

    and check_bool_expr l fl e = if expr l fl e != Bool
        then raise (Failure ("expected Boolean expression in " ^ string_of_expr e))
        else ()

    and check_stmt locals func_locals funct = function
        Block sl -> check_block locals func_locals funct sl
      | Expr e -> ignore (expr locals func_locals e)
      | Return e -> let t = expr locals func_locals e in if t = funct then () else
         raise (Failure ("return gives " ^ string_of_typ t ^ " expected " ^
                         string_of_typ funct ^ " in " ^ string_of_expr e))
      | If(p, b1, b2) -> check_bool_expr locals func_locals p; check_stmt locals func_locals funct b1; check_stmt locals func_locals funct b2
      | For(e1, e2, e3, st) -> ignore (expr locals func_locals e1); check_bool_expr locals func_locals e2;
                               ignore (expr locals func_locals e3); check_stmt locals func_locals funct st
      | While(p, s) -> check_bool_expr locals func_locals p; check_stmt locals func_locals funct s
      | Vdef(t, ids) -> raise (Failure ("declaring local variable is only allowed in blocks"))
    in

    (*Only variables defined outside any block are globals*)
    let check_global_stmt = function
        (* initds: init_dectr list *)
        Vdef(t, initds) -> List.iter
            (fun initd -> match initd with InitDectr(d, e) ->
                let tt = check_vdef StringMap.empty StringMap.empty t initd in
                let id = id_of_dectr d in
                if not (Hashtbl.mem globals id) then Hashtbl.add globals id (*fst initd*)tt
                else raise (Failure ("duplicate global " ^ id)))
            initds
      | stmt -> check_stmt StringMap.empty StringMap.empty Int stmt
    in

    List.iter check_global_stmt (List.rev statements);
    StringMap.iter (fun _ f -> if not f.checked then check_func f else ()) func_decls
