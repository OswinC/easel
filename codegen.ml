(* easel code generation *)

module L = Llvm
module A = Ast

module StringMap = Map.Make(String)

let translate (functions, statements) = 
	let context = L.global_context() in
	let the_module = L.create_module context "easel"
	and i32_t = L.i32_type context
	and float_t = L.float_type context
	and i1_t = L.i1_type context
	and void_t = L.void_type context
	and pix_t = L.i32_type context
	and arr_t = L.pointer_type 
        
	(*and func_t = L.function_type *)in

	let rec ltype_of_typ = function
	    A.Int -> i32_t
	  | A.Float -> float_t
	  | A.Bool -> i1_t
	  | A.Void -> void_t
	  | A.Pix -> pix_t 
	  | A.Arr(t)  -> 
              let t' = ltype_of_typ t in 
              arr_t t'

 	  (*| A.Func (t, l) -> i32_t(* WRONG RETURN *)*) in 

	(* TODO: declare built-in functions *)

(*	(* TODO: function definition *)
        let function_decls =
          let function_decl m fdecl =
            let name=fdecl.A.fname
            and formal_types = 
              Array.of_list (List.map (fun (t,_) -> ltype_typ t))
	(* let func_decls = *)*)
	(* TODO: function body *)
	(* TODO: declare statements *)
         
	(* Declare draw_t(), which the draw built-in function will call *)
	let draw_t = L.var_arg_function_type void_t [| i32_t; i32_t; i32_t|] in
	let the_function = L.declare_function "draw" draw_t the_module in 
	let builder = L.builder_at_end context (L.entry_block the_function) in
	(* TODO: construct function's local variables *)
(*	let locals = 
	   let add_formal m (t, n) p = L.set_value_name n p;
	 let local = L.build_alloca (ltype_of_typ t) n builder in
	    ignore (L.build_store p local builder);
	    StringMap.add n local m in

	 let add_local m (t,n) = 
	    let local_var = L.build_alloca (ltype_of_typ t) n builder in
	    StringMap.add n local_var m in

	    let formals = List.fold_left2 add_formal StringMap.empty fdecl.A.formals
		(Array.to_list (L.params the_function)) in 
		List.fold_left add_local formals fdecl.A.locals in	

	(* TODO: get value of var of formal argument *)
	let var_name n = try StringMap.find n locals
					 with Not_found -> StringMap.find n globals in 
*)

        (* Constructing code for declarations *)

	(* Constructing code for expressions *)
	let rec expr builder = function
		A.IntLit i -> L.const_int i32_t i
	  | A.FloatLit f -> L.const_float float_t f
	  | A.BoolLit b -> L.const_int i1_t (if b then 1 else 0)
	  (*| A.ArrLit a -> (* TODO: ArrLit *)*)
	  | A.PixLit (p1, p2, p3) -> let p1' = expr builder p1
                                     and p2' = expr builder p2 
                                     and p3' = expr builder p3 
				     and i = L.const_int i32_t 256 in
                                     let ii = L.build_mul i i "tmp" builder in
                                     let p2'' = L.build_mul p2' i "tmp1" builder
                                     and p3'' = L.build_mul p3' ii "tmp2" builder in
                                     let p12 = L.build_add p1' p2'' "tmp3" builder in
                                         L.build_add p12 p3'' "tmp4" builder
	  (*| A.Id id -> L.build_load (var_name id) id builder
	  | A.Noexpr -> L.const_int i32_t 0
	  | A.Binop (e1, op, e2) -> 
	  	(* TODO: define typ1 somewhere above *)
	  	let (exp1, typ1) = expr builder e1
	  	and (exp2, _) = expr builder e2 in
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
	  	) exp1 typ exp2 "tmp" builder (* TODO: is this line the correct syntax? *)
	  | A.Unop(op, e) ->
	    let (exp, t) = expr builder e in
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
	    ) exp typ "tmp" builder
	  | A.Assign (s,e) -> let exp = expr builder e in
	  					ignore (L.build_store exp (lookup s) builder); exp
	  (*TODO: EleAt, PropAcc, AnonFunc, finish Call *)
	  | A.Call (func, act) -> 
	  	let (fdef, fdecl) = StringMap.find func func_decls in 
	  	let actuals = List.rev (List.map (expr builder) (List.rev act)) in
	  	let result = (match fdecl.A.typ with A.Void -> ""
	  									   | _ -> func ^ "_result") in
	  		L.build_call fdef (Array.of_list actuals) result builder in
	  		(* TODO: add terminal if there's none *)
	  		(* TODO: statements and the builder for the statement's successor *) 
*) in
        let rec stmt builder = function
          A.Block sl -> List.fold_left stmt builder sl
        | A.Expr e -> ignore (expr builder e); builder in
        (*| A.Vdef (t, d) -> 
            names= List.map d.
            variables = List.map L.build_alloca (ltype_of_typ t) d in
            StringMap.add n variable m i
        | A.Return of expr
        | If of expr * stmt * stmt
        | For of expr * expr * expr * stmt
        | While of expr * stmt*)


    (* Build the code for each statement in the function *)
    (*let builder = expr builder (A.Block fdecl.A.body) in*)


  (*List.iter build_function_body functions;*)
  the_module
