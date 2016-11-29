(* easel code generation *)

module L = Llvm
module A = Ast 
module StringMap = Map.Make(String)

type easel_env = {
    locals: (L.llvalue * (A.typ * A.dectr)) StringMap.t;
    builder: L.llbuilder; 
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
      | A.DecId(id) -> L.const_int (lltype_of_typ t) 0 
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

    let globals = Hashtbl.create 8 in

    let global_var t = function A.InitDectr(dectr, init) ->
      (* TODO: if init is not empty, parse it and use it *)
      let inst = llval_of_dectr t dectr in
      let n = id_of_dectr dectr in
      Hashtbl.add globals n (L.define_global n inst the_module, (t, dectr))
    in

    let local_var t env = function A.InitDectr(dectr, init) ->
      (* TODO: if init is not empty, parse it and use it *)
      (*let inst = llval_of_dectr t dectr in*)
      let n = id_of_dectr dectr in
      let loc = L.build_alloca (lltype_of_typ t) n env.builder in
      let locals = StringMap.add n (loc, (t, dectr)) env.locals in
      { locals = locals; builder = env.builder}
    in

	(* built-in functions *)
    let extfunc_draw_def_t = L.var_arg_function_type i32_t [||] in
    let extfunc_draw_def = L.declare_function "draw_default" extfunc_draw_def_t the_module in 
    let extfunc_do_draw_t = L.var_arg_function_type i32_t [|ptr_t i32_t; i32_t; i32_t; i32_t; i32_t|] in
    let extfunc_do_draw = L.declare_function "do_draw" extfunc_do_draw_t the_module in 
    let extfunc_printf_t = L.var_arg_function_type i32_t [| L.pointer_type i8_t |] in
    let extfunc_printf = L.declare_function "printf" extfunc_printf_t the_module in

(*	(* TODO: function definition *)
        let function_decls =
          let function_decl m fdecl =
            let name=fdecl.A.fname
            and formal_types = 
              Array.of_list (List.map (fun (t,_) -> lltype_typ t))
	(* let func_decls = *)*)
	(* TODO: function body *)
	(* TODO: declare statements *)
         
	(*let builder = L.builder_at_end context (L.entry_block the_function) in*)
	(* TODO: construct function's local variables *)
(*	let locals = 
	   let add_formal m (t, n) p = L.set_value_name n p;
	 let local = L.build_alloca (lltype_of_typ t) n builder in
	    ignore (L.build_store p local builder);
	    StringMap.add n local m in

	 let add_local m (t,n) = 
	    let local_var = L.build_alloca (lltype_of_typ t) n builder in
	    StringMap.add n local_var m in

	    let formals = List.fold_left2 add_formal StringMap.empty fdecl.A.formals
		(Array.to_list (L.params the_function)) in 
		List.fold_left add_local formals fdecl.A.locals in	
*)

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
      (*
	  | A.Noexpr -> L.const_int i32_t 0
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
	  | A.Assign (s,e) -> let exp = expr env e in
	  					ignore (L.build_store exp (lookup env s) env.builder); exp
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
					  A.Id s -> lookup env s
					(*| A.EleAt(arr, ind) -> let a = expr env arr in 
							       L.build_gep a [| L.const_int i32_t 0; expr env ind |] a env.builder*)
					)
			    and e2' = expr env e2 in
			  ignore(L.build_store e2' (fst e1') env.builder); e2'
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
            
       (* | A.Return of expr
        | If of expr * stmt * stmt
        | For of expr * expr * expr * stmt
        | While of expr * stmt*)
    in


    (* Build the code for each statement in the function *)
    (*let builder = expr builder (A.Block fdecl.A.body) in*)

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
        let env = {locals = StringMap.empty; builder = builder} in

        List.fold_left global_stmt env sl;

        (* Add a return if the last block falls off the end *)
        add_terminal builder (L.build_ret (L.const_int i32_t 0))
    in
    build_main_function (List.rev statements);
    (*List.iter build_function_body functions;*)
    the_module
