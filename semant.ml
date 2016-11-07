(* Semantic checking for easel compiler *)

open Ast

module StringMap = Map.Make(String)

(* The semantic checker will return void if successful and will throw an exception otherwise *) 

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
	    if lvalt == rvalt then lvalt else raise err
	in

	(* Check and build function table *)
	let functions =
        { typ = Void; fname = "draw"; formals = [(Pix, "name[][]"); (Float, "x"); (Float, "y")];
          body = [] }::
        { typ = Void; fname = "drawout"; formals = [(Pix, "name[][]")];
	      body = [] }::
	    { typ = Float; fname = "tan"; formals = [(Float, "x")];
          body = [] }::
	    { typ = Float; fname = "sin"; formals = [(Float, "x")];
          body = [] }::
 	    { typ = Float; fname = "cos"; formals = [(Float, "x")];
          body = [] }::
	    { typ = Float; fname = "log"; formals= [(Float, "base"); (Float, "value")];
          body = [] }::
	    { typ = Float; fname = "rand"; formals = [];
          body = [] }:: functions
	in

    let rec typ_of_bind = function
          (t, DecId(_)) -> t
        | (t, DecArr(d, _)) -> typ_of_bind (Arr(t), d)
    in

    let func_sign fd =
        string_of_typ fd.typ ^ fd.name ^
        List.fold_left (fun s fm -> s ^ string_of_typ (typ_of_bind fm)) "" fd.formals
    in
	(* Check functions *)
	(* !! I'm assuming here that we're changing the names for overloaded functions
     * prior to checking for duplicates but maybe my logic is off and there's
     * supposed to be more code here that will simulatenously concatenate the name
     * and prototype???
     * Yes, it should be the function signature that is used to check for duplicate
     * so overloaded functions won't be complained. *)
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
