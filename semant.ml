(* Semantic checking for easel compiler *)

open Ast

module StringMap = Map.Make(String)

(* The semantic checker will return void if successful and will throw an exception otherwise *)

let check (globals, functions) = 
	
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

	(* Check globals *)
	List.iter (check_void (fun n -> "illegal void global " ^ n)) globals;
	report_dup (fun n -> "duplicate global " ^ n) (List.map snd globals);

	(* Check functions *)
	(* !! I'm assuming here that we're changing the names for overloaded functions prior to checking for duplicates but maybe my logic is off and there's supposed to be more code here that will simulatenously concatenate the name and prototype??? *)
	report_dup (fun n -> "duplicate function " ^ n)
	  (List.map (fun fd -> fd.fname) functions);

	(* Check named functions *)
	let built_in_decls = StringMap.add "draw"
	    { typ = Void; fname = "draw"; formals = [(Pix[][], "name"); (Float, "x"); (Float, "y")];		  body = [] } (StringMap.add "drawout"
	    { typ = Void; fname = "drawout"; formals = [(Pix[][], "name")];
	      body = [] } (StringMap.add "tan"
	    { typ = Float; fname = "tan"; formals = [(Float, "x")];
	      body = [] } (StringMap.add "sin"
	    { typ = Float; fname = "sin"; formals = [(Float, "x")];
	      body = [] } (StringMap.add "cos"
 	    { typ = Float; fname = "cos"; formals = [(Float, "x")];
	      body = [] } (StringMap.add "log"
	    { typ = Float; fname = "log"; formals= [(Float, "base"); (Float, "value")];
	      body = [] } (StringMap.add "rand"
	    { typ = Float; fname = "rand"; formals = [];
	      body = [] } ))))))
	in

	let func_decls = List.fold_left (fun m fd -> StringMap.add fd.fname fd m) 
		built_in_decels functions 
	in 

	let func_decl s = try StringMap.find s func_decls
	    with Not_found -> raise (Failure ("unrecognized function " ^ s))
	in

	let _ = func_decl "main" in	
