(* easel code generation *)

module L = Llvm
module A = Ast 
module StringMap = Map.Make(String)

type easel_env = {
    locals: (L.llvalue * (A.typ * A.dectr)) StringMap.t;
    builder: L.llbuilder; 
    the_func: L.llvalue; 
}

let translate (functions, statements) = 
	let context = L.global_context() in
	let the_module = L.create_module context "easel"
	and i32_t = L.i32_type context
        and i8_t = L.i8_type   context
	and float_t = L.float_type context
	and i1_t = L.i1_type context
	and void_t = L.void_type context
	and pix_t = L.i32_type context
	and arr_t t n = L.array_type t n 
	and ptr_t t = L.pointer_type t
        
	(*and func_t = L.function_type *)in

	let rec lltype_of_typ = function
	    A.Int -> i32_t
	  | A.Float -> float_t
	  | A.Bool -> i1_t
	  | A.Void -> void_t
	  | A.Pix -> pix_t 
	  | A.Arr(t) -> 
              let t' = lltype_of_typ t in 
              ptr_t t'


 	  (*| A.Func (t, l) -> i32_t(* WRONG RETURN *)*) in 

    let rec lltype_of_dectr t = function
        A.DecArr(d, l) -> arr_t (lltype_of_dectr t d) l
      | A.DecId(_) -> lltype_of_typ t
    in

    let rec llval_of_dectr t = function
        (* 1-D: L.const_array pix_t [|L.const_int pix_t 0; L.const_int pix_t 0; ...|]*)
        (* 2-D: L.const_array (arr_t pix_t 10) [|... |]*)
        (* 3-D: L.const_array (arr_t (arr_t 5) 10) [|... |]*)
        (* Scalar: L.const_int pix_t 0*)
        A.DecArr(d, l) -> L.const_array (lltype_of_dectr t d) (Array.make l (llval_of_dectr t d))
      | A.DecId(id) -> (match t with 
                          A.Int -> L.const_int (lltype_of_typ t) 0 
                        | A.Pix -> L.const_int (lltype_of_typ t) 0 
                        | A.Bool -> L.const_int (lltype_of_typ t) 0 
                        | A.Float -> L.const_float (lltype_of_typ t) 2.4 
                       )

    in

    let rec id_of_dectr = function
        A.DecId(id) -> id
      | A.DecArr(d, _) -> id_of_dectr d
    in 

    let sub_dectr = function
        A.DecId(_) as d -> d
      | A.DecArr(d, _) -> d
    in 

    let decarr_len = function
        A.DecArr(_, l) -> l
      | A.DecId(id) -> raise (Failure (id ^ " is not an array"))
    in 

    let rec get_arr_id = function
        A.Id(id) -> id
      | A.EleAt(id,_) -> get_arr_id id
    in

    let globals = Hashtbl.create 8 in

    let global_var t = function A.InitDectr(dectr, init) ->
      (* TODO: if init is not empty, parse it and use it *)
      let inst = llval_of_dectr t dectr in
      let n = id_of_dectr dectr in
      Hashtbl.add globals n (L.define_global n inst the_module, (t, dectr))
    in

    let function_decls = 
      let function_decl m fdecl =
	let name = fdecl.A.fname
	and formal_t = Array.of_list (List.map (fun (t,_) -> lltype_of_typ t) fdecl.A.formals) in
	let ftype = L.function_type (lltype_of_typ fdecl.A.typ) formal_t in
	  StringMap.add name (L.define_function name ftype the_module, fdecl) m in
	List.fold_left function_decl StringMap.empty functions in	
        
	(*let build_function_body fdecl = 
        let (the_function, _) = StringMap.find fdecl.A.fname function_decls in
        let builder = L.builder_at_end context (L.entry_block the_function) in 
    *)
    let local_var t env = function A.InitDectr(dectr, init) ->
      (* TODO: if init is not empty, parse it and use it *)
      (*let inst = llval_of_dectr t dectr in*)
      let n = id_of_dectr dectr in
      let loc = L.build_alloca (lltype_of_typ t) n env.builder in
      let locals = StringMap.add n (loc, (t, dectr)) env.locals in
      { locals = locals; builder = env.builder; the_func = env.the_func}
    in

	(* built-in functions *)
    let extfunc_draw_def_t = L.var_arg_function_type i32_t [||] in
    let extfunc_draw_def = L.declare_function "draw_default" extfunc_draw_def_t the_module in 
    let extfunc_do_draw_t = L.var_arg_function_type i32_t [|ptr_t i32_t; i32_t; i32_t; i32_t; i32_t|] in
    let extfunc_do_draw = L.declare_function "do_draw" extfunc_do_draw_t the_module in 
    let extfunc_printf_t = L.var_arg_function_type i32_t [| L.pointer_type i8_t |] in
    let extfunc_printf = L.declare_function "printf" extfunc_printf_t the_module in

	(* TODO: lookup local table before go into globals *)
	let lookup env n = try StringMap.find n env.locals
					 with Not_found -> Hashtbl.find globals n in 

        (* Constructing code for declarations *)

	(* Constructing code for expressions *)
	let rec expr env = function
		A.IntLit i -> L.const_int i32_t i
	  | A.FloatLit f -> L.const_float float_t f
	  | A.BoolLit b -> L.const_int i1_t (if b then 1 else 0)
	  (*| A.ArrLit a -> (* TODO: ArrLit *)*)
	  | A.PixLit (p1, p2, p3) -> let p1' = expr env p1
                                     and p2' = expr env p2 
                                     and p3' = expr env p3 
				     and i = L.const_int i32_t 256 in
                                     let ii = L.build_mul i i "tmp" env.builder in
                                     let p2'' = L.build_mul p2' i "tmp" env.builder
                                     and p3'' = L.build_mul p3' ii "tmp" env.builder in
                                     let p12 = L.build_add p1' p2'' "tmp" env.builder in
                                         L.build_add p12 p3'' "tmp" env.builder
      | A.Id id -> L.build_load (fst (lookup env id)) id env.builder
	  | A.Noexpr -> L.const_int i32_t 0
      (*
	  | A.Binop (e1, op, e2) -> 
	  	(* TODO: define typ1 somewhere above *)
	  	let (exp1, typ1) = expr env e1
	  	and (exp2, _) = expr env e2 in
	  	(match op with 
	  	  A.Add -> if typ1 = A.Int then L.build_add else L.build_fadd
	  	| A.Sub -> if typ1 = A.Int then L.build_sub else L.build_fsub
	  	| A.Mult -> if typ1 = A.Int then L.build_mul else L.build_fmul
	  	| A.Div -> if typ1 = A.Int then L.build_sdiv else L.build_fdiv
	  	| A.Mod -> L.build_urem 
	  	| A.Pow -> L.powi
	  	| A.Equal -> if typ1 = A.Int then (L.build_icmp L.Icmp.Eq)
	  				 else (L.build_fcmp L.Fcmp.Eq)
	  	| A.Neq -> if typ1 = A.Int then (L.build_icmp L.Icmp.Ne)
	  			   else (L.build_fcmp L.Fcmp.One)
	  	| A.Less -> if typ1 = A.Int then (L.build_icmp L.Icmp.Slt)
	  				else (L.build_fcmp L.Fcmp.Olt)
	  	| A.Leq -> if typ1 = A.Int then (L.build_icmp L.Icmp.Sle)
	  			   else (L.build_fcmp L.Fcmp.Ole)
	  	| A.Greater -> if typ1 = A.Int then (L.build_icmp L.Icmp.Sgt)
	  				   else (L.build_fcmp L.Fcmp.Ogt)
	  	| A.Geq -> if typ1 = A.Int then (L.build_icmp L.Icmp.Sge)
	  			   else (L.build_fcmp L.Fcmp.Oge)
	  	| A.And -> L.build_and
	  	| A.Or -> L.build_or
	  	) exp1 typ exp2 "tmp" env.builder (* TODO: is this line the correct syntax? *)
	  | A.Unop(op, e) ->
	    let (exp, t) = expr env e in
	    (match op with
	    	A.Neg -> L.build_neg
	      | A.Not -> L.build_not
	      | A.Inc -> if t = A.Int then (L.build_add 1) (* not positive this is correct *)
	      			 else (L.build_fadd 1)
	      | A.Dec -> if t = A.Int then (L.build_sub 1)
	      			 else (L.build_fsub 1)
	      (* TODO: UMult, UDive, UPow
	      | A.UMult ->
	      | A.UDiv ->
	      | A.UPow -> *)
	    ) exp typ "tmp" env.builder
	  (*TODO: EleAt, PropAcc, AnonFunc, finish Call *)
	  | A.Call (func, act) -> 
	  	let (fdef, fdecl) = StringMap.find func func_decls in 
	  	let actuals = List.rev (List.map (expr env) (List.rev act)) in
	  	let result = (match fdecl.A.typ with A.Void -> ""
	  									   | _ -> func ^ "_result") in
	  		L.build_call fdef (Array.of_list actuals) result env.builder in
	  		(* TODO: add terminal if there's none *)
	  		(* TODO: statements and the builder for the statement's successor *) *)
      | A.Assign(e1, e2) -> let e1' = (match e1 with 
					  A.Id s -> fst (lookup env s)
					| A.EleAt(arr, ind) -> (match arr with 
					    A.Id s -> L.build_gep (fst (lookup env s)) [|L.const_int i32_t 0; expr env ind|] s env.builder
					  | A.EleAt(s, l) -> let s' = get_arr_id s in 
					    L.build_gep (fst (lookup env s')) [|L.const_int i32_t 0; expr env ind; expr env l|] s' env.builder
					  )
					)
			    and e2' = expr env e2 in
			  ignore(L.build_store e2' e1' env.builder); e2'
      | A.EleAt(arr, ind) -> (match arr with
          A.Id s -> L.build_load (L.build_gep (fst (lookup env s )) [|L.const_int i32_t 0; expr env ind|] s env.builder) s env.builder
        | A.EleAt(s, l) -> let s' = get_arr_id s in
          L.build_load (L.build_gep (fst (lookup env s')) [|L.const_int i32_t 0; expr env ind; expr env l|] s' env.builder) s' env.builder
        )
      (* Call external functions *)
      (* int draw() *)
      | A.Call (Id("draw"), []) ->
        L.build_call extfunc_draw_def [||] "draw_def" env.builder
      | A.Call (Id("draw"), [Id(cid); e2; e3]) ->
        let c = lookup env cid in
        let c_llval = fst c in
        let c_col = snd (snd c) in
        let c_row = sub_dectr c_col in
        let w = decarr_len c_row in
        let h = decarr_len c_col in
        let zero = L.const_int i32_t 0 in
        let c_ptr = L.build_in_bounds_gep c_llval [|zero; zero; zero|] "cnvstmp" env.builder in
        L.build_call extfunc_do_draw [| c_ptr; L.const_int i32_t w; L.const_int i32_t h;
                                        expr env e2; expr env e3 |] "do_draw" env.builder
      | A.Call (Id("print"), [e]) -> 
        let int_format_str = L.build_global_stringptr "%d\n" "fmt" env.builder in
        L.build_call extfunc_printf [| int_format_str ; (expr env e) |] "printf" env.builder
      | A.Call (Id("printfl"), [e]) -> 
        let float_format_str = L.build_global_stringptr "%f\n" "fmt" env.builder in
        L.build_call extfunc_printf [| float_format_str ; (expr env e) |] "printf" env.builder
    in

    (* Invoke "f builder" if the current block doesn't already
       have a terminal (e.g., a branch). *)
    let add_terminal builder f =
      match L.block_terminator (L.insertion_block builder) with
	Some _ -> ()
      | None -> ignore (f builder) in
	
    let rec stmt env = function
        (* Discard the locals built in the inner block *)
        A.Block sl -> ignore(List.fold_left stmt env sl); env
      | A.Expr e -> ignore (expr env e); env
      | A.Vdef (t, initds) ->
        List.fold_left (local_var t) env initds
      | A.While (pred, body) -> 
        let pred_bb = L.append_block context "while" env.the_func in
        ignore (L.build_br pred_bb env.builder);

        let body_bb = L.append_block context "while_body" env.the_func in
        (*todo!*)
        let body_env = {
            locals = env.locals;
            builder = (L.builder_at_end context body_bb);
            the_func = env.the_func } in
        add_terminal (stmt body_env body).builder
        (L.build_br pred_bb);

        let pred_env = {
            locals = env.locals;
            builder = (L.builder_at_end context pred_bb);
            the_func = env.the_func } in
        let pred_v = expr pred_env pred in

        let merge_bb = L.append_block context "merge" env.the_func in 
        let cmp_v = L.build_icmp L.Icmp.Ne pred_v (L.const_int i32_t 0) "cmp" pred_env.builder in
        ignore (L.build_cond_br cmp_v body_bb merge_bb pred_env.builder);
        { locals = env.locals; builder = L.builder_at_end context merge_bb; the_func = env.the_func }
       (* | A.Return of expr
        | If of expr * stmt * stmt
        | For of expr * expr * expr * stmt
        | While of expr * stmt*)
    in

    let global_stmt env = function
        (* initds: init_dectr list *)
          A.Vdef(t, initds) -> List.iter (global_var t) initds; env
        | st -> stmt env st
    in

    let build_main_function sl =
        (* Define the main function for executing global statements *)
        let ftype_main = L.function_type i32_t [||] in
        let main_func = L.define_function "main" ftype_main the_module in
        let builder = L.builder_at_end context (L.entry_block main_func) in
        let env = {locals = StringMap.empty; builder = builder; the_func = main_func} in 

        let end_env = List.fold_left global_stmt env sl in

        (* Add a return if the last block falls off the end *)
        add_terminal end_env.builder (L.build_ret (L.const_int i32_t 0))
    in
    build_main_function (List.rev statements);
    (*List.iter build_function_body functions;*)
    the_module
