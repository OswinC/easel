(* easel code generation *)

module L = Llvm
module A = Ast 
module StringMap = Map.Make(String)

type easel_env = {
    locals: (L.llvalue * (A.typ * A.dectr)) StringMap.t;
    builder: L.llbuilder; 
    the_func: L.llvalue; 
    ret_typ: A.typ;
}

let translate (functions, statements) = 
	let context = L.global_context() in
	let the_module = L.create_module context "easel"
	and i32_t = L.i32_type context
        and i8_t = L.i8_type   context
	and float_t = L.double_type context
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
                        | A.Float -> L.const_float (lltype_of_typ t) 0.0 
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

    let init_var t dectr = function 
        A.IntLit i -> L.const_int i32_t i
      | A.FloatLit f -> L.const_float float_t f
      | A.BoolLit b -> L.const_int i1_t (if b then 1 else 0)
      (*| A.PixLit (r, g, b) ->  
      | A.ArrLit -> *)
      | _ -> llval_of_dectr t dectr 
    in 

    let globals = Hashtbl.create 8 in

    let global_var t = function A.InitDectr(dectr, init) ->
      let inst = init_var t dectr init in 
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
        
    let local_var t env = function A.InitDectr(dectr, init) ->
      let n = id_of_dectr dectr in
      let loc = L.build_alloca (lltype_of_typ t) n env.builder in
      let inst = init_var t dectr init in 
      let _ = L.build_store inst loc env.builder in 
      let locals = StringMap.add n (loc, (t, dectr)) env.locals in
      { env with locals = locals }
    in

	(* built-in functions *)
    let extfunc_draw_def_t = L.var_arg_function_type i32_t [||] in
    let extfunc_draw_def = L.declare_function "draw_default" extfunc_draw_def_t the_module in 
    let extfunc_do_draw_t = L.var_arg_function_type i32_t [|ptr_t i32_t; i32_t; i32_t; i32_t; i32_t|] in
    let extfunc_do_draw = L.declare_function "do_draw" extfunc_do_draw_t the_module in 
    let extfunc_printf_t = L.var_arg_function_type i32_t [| L.pointer_type i8_t |] in
    let extfunc_printf = L.declare_function "printf" extfunc_printf_t the_module in
    let extfunc_pow_t = L.function_type float_t [| float_t; float_t |] in
    let extfunc_pow = L.declare_function "llvm.pow.f64" extfunc_pow_t the_module in
    let pow_call b e n bdr = L.build_call extfunc_pow [|b; e|] n bdr in
    let extfunc_powi_t = L.function_type float_t [| float_t; i32_t |] in
    let extfunc_powi = L.declare_function "llvm.powi.f64" extfunc_powi_t the_module in
    let powi_call b e n bdr = L.build_call extfunc_powi [|b; e|] n bdr in
    let extfunc_sin_t = L.var_arg_function_type float_t [|float_t|] in 
    let extfunc_sin = L.declare_function "sin" extfunc_sin_t the_module in
    let extfunc_cos_t = L.var_arg_function_type float_t [|float_t|] in 
    let extfunc_cos = L.declare_function "cos" extfunc_cos_t the_module in
    let extfunc_tan_t = L.var_arg_function_type float_t [|float_t|] in 
    let extfunc_tan = L.declare_function "tan" extfunc_tan_t the_module in 

	let lookup env n = try StringMap.find n env.locals
					 with Not_found -> Hashtbl.find globals n in 

        (* Constructing code for declarations *)

	(* Constructing code for expressions *)
	let rec expr env = function
	    A.IntLit i -> L.const_int i32_t i
	  | A.FloatLit f -> L.const_float float_t f
	  | A.BoolLit b -> L.const_int i1_t (if b then 1 else 0)
	  (*| A.ArrLit a -> (* TODO: ArrLit *)*)
	  | A.PixLit (r_e, g_e, b_e) -> let r_v = expr env r_e
                                    and g_v = expr env g_e
                                    and b_v = expr env b_e in
                                    let shift_r = L.const_int i32_t 16777216 (* left shift for 24 bits *)
                                    and shift_g = L.const_int i32_t 65536 (* left shift for 16 bits *)
                                    and shift_b = L.const_int i32_t 256 in (* left shift for 8 bits *)
                                    let r_v' = L.build_mul r_v shift_r "tmp" env.builder
                                    and g_v' = L.build_mul g_v shift_g "tmp" env.builder
                                    and b_v' = L.build_mul b_v shift_b "tmp" env.builder in
                                    let p_v' = L.build_add r_v' g_v' "tmp" env.builder in
                                        L.build_add p_v' b_v' "tmp" env.builder
          | A.Id id -> L.build_load (fst (lookup env id)) id env.builder
	  | A.Noexpr -> L.const_int i32_t 0
	  | A.Binop (e1, op, e2) -> 
            let exp1 = expr env e1
            and exp2 = expr env e2 in
            let typ1 = L.string_of_lltype (L.type_of exp1) 
            and typ2 = L.string_of_lltype (L.type_of exp2) in 
            let build_op_by_type opf opi =
                (match (typ1, typ2) with
                  ("double", "double") -> opf
                | ("i32", "i32") -> opi
                | ("double", "i32") ->
                    (fun e1 e2 n bdr -> let e2' = L.build_sitofp e2 float_t "tmp" bdr in
                                        opf e1 e2' "tmp" bdr)
                | ("i32", "double") ->
                    (fun e1 e2 n bdr -> let e1' = L.build_sitofp e1 float_t "tmp" bdr in
                                        opf e1' e2 "tmp" bdr)
                ) in
            (match op with
              A.Add -> build_op_by_type L.build_fadd L.build_add
            | A.Sub -> build_op_by_type  L.build_fsub L.build_sub
            | A.Mult -> build_op_by_type L.build_fmul L.build_mul
            | A.Div -> build_op_by_type  L.build_fdiv L.build_sdiv
            | A.Mod -> build_op_by_type  L.build_frem L.build_srem
            | A.Pow -> (match (typ1, typ2) with
                        ("double", "i32") -> powi_call
                        | ("double", "double") -> pow_call
                        | ("i32", "double") -> (fun e1 e2 n bdr -> let e1' = L.build_sitofp e1 float_t "tmp" bdr in
                                                pow_call e1' e2 "tmp" bdr)
                        | ("i32", "i32") -> (fun e1 e2 n bdr -> let e1' = L.build_sitofp e1 float_t "tmp" bdr in
                                                powi_call e1' e2 "tmp" bdr)
                        )
            | A.Equal -> build_op_by_type (L.build_fcmp L.Fcmp.Oeq) (L.build_icmp L.Icmp.Eq)
            | A.Neq -> build_op_by_type (L.build_fcmp L.Fcmp.One) (L.build_icmp L.Icmp.Ne)
            | A.Less -> build_op_by_type (L.build_fcmp L.Fcmp.Olt) (L.build_icmp L.Icmp.Slt)
            | A.Leq -> build_op_by_type (L.build_fcmp L.Fcmp.Ole) (L.build_icmp L.Icmp.Sle)
            | A.Greater -> build_op_by_type (L.build_fcmp L.Fcmp.Ogt) (L.build_icmp L.Icmp.Sgt)
            | A.Geq -> build_op_by_type (L.build_fcmp L.Fcmp.Oge) (L.build_icmp L.Icmp.Sge)
            | A.And -> L.build_and
            | A.Or -> L.build_or
            ) exp1 exp2 "tmp" env.builder 
	  | A.Unop(op, e) ->
	        let exp = expr env e in
                let typ = L.string_of_lltype (L.type_of exp) in
        (*let fincrement b n en = L.build_fadd b (L.const_float float_t 1.0) n en in*)
        (*let iincrement b n en = L.build_add b (L.const_int i32_t 1) n en in*)
	        (match op with
	    	  A.Neg -> L.build_neg exp "tmp" env.builder
	        | A.Not -> L.build_not exp "tmp" env.builder
                | A.Inc -> (match typ with
                    "double" -> expr env (A.Assign(e, A.Binop(e, A.Add, A.FloatLit(1.0))))
                  | _ -> expr env (A.Assign(e, A.Binop(e, A.Add, A.IntLit(1))))
                )
                | A.Dec -> (match typ with
                    "double" -> expr env (A.Assign(e, A.Binop(e, A.Sub, A.FloatLit(1.0))))
                  | _ -> expr env (A.Assign(e, A.Binop(e, A.Sub, A.IntLit(1))))
	        )
                | A.UMult -> expr env (A.Assign(e, A.Binop(e, A.Mult, e)))
	        | A.UDiv -> expr env(A.Assign(e, A.Binop(e, A.Div, e)))
                (*| A.UPow -> expr env(A.Assign(e, A.Binop(e, A.Pow, e)))*)
	        ) 
	  (*TODO: AnonFunc, finish Call *)
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
      | A.PropAcc(e,s)-> (match s with 
          "red" -> let v = expr env e 
                   and shift = L.const_int i32_t 24 in
                     L.build_ashr v shift "tmp" env.builder
        | "green" -> let v = expr env e
                     and shift_r = L.const_int i32_t 24  
                     and shift_g = L.const_int i32_t 16 in
                     let r_v = L.build_ashr v shift_r "tmp" env.builder 
                     and g_v = L.build_ashr v shift_g "tmp" env.builder 
                     and shift = L.const_int i32_t 256 in 
                     let r_v' = L.build_mul r_v shift "tmp" env.builder in
                       L.build_sub g_v r_v' "tmp" env.builder
        | "blue" -> let v = expr env e
                    and shift_g = L.const_int i32_t 16
                    and shift_b = L.const_int i32_t 8 in
                    let g_v = L.build_ashr v shift_g "tmp" env.builder
                    and b_v = L.build_ashr v shift_b "tmp" env.builder
                    and shift_g' = L.const_int i32_t 256 in
                    let g_v' = L.build_mul g_v shift_g' "tmp" env.builder in
                      L.build_sub b_v g_v' "tmp" env.builder
        (* "size" -> TODO *)
        )
      (* Call external functions *)
      (* int draw() *)
      | A.Call (A.Id("draw"), []) ->
        L.build_call extfunc_draw_def [||] "draw_def" env.builder
      | A.Call (A.Id("draw"), [A.Id(cid); e2; e3]) ->
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
      | A.Call (A.Id("print"), [e]) -> 
        let int_format_str = L.build_global_stringptr "%d\n" "fmt" env.builder in
        L.build_call extfunc_printf [| int_format_str ; (expr env e) |] "printf" env.builder
      | A.Call (A.Id("printfl"), [e]) -> 
        let float_format_str = L.build_global_stringptr "%f\n" "fffmt" env.builder in
        L.build_call extfunc_printf [| float_format_str ; (expr env e) |] "printf" env.builder
        (* TODO: Overloading and passing arrays for function calls *)
      | A.Call (A.Id("sin"), [e]) ->
	 L.build_call extfunc_sin [|expr env e|] "sin" env.builder
      | A.Call (A.Id("cos"), [e]) ->
         L.build_call extfunc_cos [|expr env e|] "cos" env.builder
      | A.Call (A.Id("tan"), [e]) ->
         L.build_call extfunc_tan [|expr env e|] "tan" env.builder
      | A.Call (A.Id(func), act) -> 
          let (fdef, fdecl) = StringMap.find func function_decls in 
          let actuals = List.rev (List.map (expr env) (List.rev act)) in
          let result = (match fdecl.A.typ with A.Void -> ""
                                             | _ -> func ^ "_result") in
              L.build_call fdef (Array.of_list actuals) result env.builder
    in

    (* Invoke "f builder" if the current block doesn't already
       have a terminal (e.g., a branch). *)
    let add_terminal builder f =
      match L.block_terminator (L.insertion_block builder) with
	Some _ -> ()
      | None -> ignore (f builder) in
	
    let rec stmt env = function
        (* Discard the locals built in the inner block *)
        A.Block sl -> List.fold_left stmt env sl
      | A.Expr e -> ignore (expr env e); env
      | A.Vdef (t, initds) ->
        List.fold_left (local_var t) env initds
      | A.If (pred, then_stmt, else_stmt) ->
          let bool_val = expr env pred in 
	  let merge_bb = L.append_block context "merge" env.the_func in

	  let then_bb = L.append_block context "then" env.the_func in
	  let then_env = { env with builder = (L.builder_at_end context then_bb) } in
          add_terminal (stmt then_env then_stmt).builder
          (L.build_br merge_bb);

          let else_bb = L.append_block context "else" env.the_func in
	  let else_env = { env with builder = (L.builder_at_end context else_bb) } in
	  add_terminal (stmt else_env else_stmt).builder
	  (L.build_br merge_bb);

	  ignore (L.build_cond_br bool_val then_bb else_bb env.builder);
	  { env with builder = (L.builder_at_end context merge_bb) }
      | A.While (pred, body) -> 
        let pred_bb = L.append_block context "while" env.the_func in
        ignore (L.build_br pred_bb env.builder);

        let body_bb = L.append_block context "while_body" env.the_func in
        let body_env = { env with builder = (L.builder_at_end context body_bb) } in
        add_terminal (stmt body_env body).builder
        (L.build_br pred_bb);

        let pred_env = { env with builder = (L.builder_at_end context pred_bb) } in
        let pred_v = expr pred_env pred in
        let pred_t = L.type_of pred_v in

        let merge_bb = L.append_block context "merge" env.the_func in 
        let cmp_v =
            if pred_t = i32_t then L.build_icmp L.Icmp.Ne pred_v (L.const_int i32_t 0) "cmp" pred_env.builder
            else pred_v in
        ignore (L.build_cond_br cmp_v body_bb merge_bb pred_env.builder);
        { env with builder = (L.builder_at_end context merge_bb) }

      | A.For (e1, e2, e3, body) -> stmt env
	      ( A.Block [A.Expr e1 ; A.While (e2, A.Block [body ; A.Expr e3]) ] )

      | A.Return e ->
			let e' = expr env e in
            let e_t = L.type_of e' in
			ignore (match (env.ret_typ, e_t) with
			  (A.Void, _) -> L.build_ret_void env.builder
			| (A.Int, float_t) -> let e'' = L.build_fptosi e' i32_t "tmp" env.builder in
                                   L.build_ret e'' env.builder
			| (A.Float, i32_t) -> let e'' = L.build_sitofp e' float_t "tmp" env.builder in
                                   L.build_ret e'' env.builder
			| _ -> L.build_ret (expr env e) env.builder
            ); env
       (* | If of expr * stmt * stmt *)
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
        let env = { locals = StringMap.empty; builder = builder; the_func = main_func; ret_typ = A.Int } in 

        let end_env = List.fold_left global_stmt env sl in

        (* Add a return if the last block falls off the end *)
        add_terminal end_env.builder (L.build_ret (L.const_int i32_t 0))
    in

    let build_function_body fdecl = 
        let (the_function, _) = StringMap.find fdecl.A.fname function_decls in
        let builder = L.builder_at_end context (L.entry_block the_function) in 
        let add_formal m (ty, dectr) p =
            let n = id_of_dectr(dectr) in
            L.set_value_name n p;
            let local = L.build_alloca (lltype_of_typ ty) n builder in
            ignore (L.build_store p local builder);
            StringMap.add n (local, (ty, dectr)) m
        in
        let formals = List.fold_left2 add_formal StringMap.empty fdecl.A.formals
            (Array.to_list (L.params the_function))
        in
        let env = { locals = formals; builder = builder; the_func = the_function; ret_typ = fdecl.A.typ } in

        (* Build the code for each statement in the function *)
        let env = stmt env (A.Block fdecl.A.body) in

        (* Add a return if the last block falls off the end *)
        add_terminal env.builder (match fdecl.A.typ with
          A.Void -> L.build_ret_void
        | A.Float -> L.build_ret (L.const_float float_t 0.)
        | t -> L.build_ret (L.const_int (lltype_of_typ t) 0))
    in
    
    build_main_function (List.rev statements);
    List.iter build_function_body functions;
    the_module
