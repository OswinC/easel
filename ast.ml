(* Abstract Syntax Tree and functions for printing it *)

type op = Add | Sub | Mult | Div | Mod | Pow | Equal | Neq | Less | Leq | Greater | Geq |
          And | Or

and uop = Neg | Not | Inc | Dec | UMult | UDiv | UPow

and typ = Int | Float | Bool | Void | Pix | Func of typ * typ list | ArrRef of typ

and dectr =
    DecId of string
  | DecArr of dectr * int (* declarator * array length *)

and bind = typ * dectr

and expr =
    IntLit of int
  | FloatLit of float
  | BoolLit of bool
  | ArrLit of expr list
  | PixLit of expr list
  | Id of string
  | Binop of expr * op * expr
  | Unop of uop * expr
  | Assign of expr * expr
  | Call of expr * expr list
  | EleAt of expr * expr
  | PropAcc of expr * string (* Access property "string" of "expr" *)
  | AnonFunc of func_decl
  | Noexpr

and init_dectr = InitDectr of dectr * expr (* dectr * Initializer *)

and stmt =
    Block of stmt list
  | Expr of expr
  | Vdef of typ * init_dectr list
  | Return of expr
  | If of expr * stmt * stmt
  | For of expr * expr * expr * stmt
  | While of expr * stmt

and func_decl = {
    typ : typ;
    fname : string;
    formals : bind list;
    body : stmt list;
    checked: bool;
  }

and program = func_decl list * stmt list

(* Pretty-printing functions *)

let rec string_of_op = function
    Add -> "+"
  | Sub -> "-"
  | Mult -> "*"
  | Div -> "/"
  | Mod -> "%"
  | Pow -> "^"
  | Equal -> "=="
  | Neq -> "!="
  | Less -> "<"
  | Leq -> "<="
  | Greater -> ">"
  | Geq -> ">="
  | And -> "&&"
  | Or -> "||"

and string_of_typ = function
    Int -> "int"
  | Float -> "float"
  | Bool -> "bool"
  | Void -> "void"
  | Pix -> "pix"
  | Func(t, tl) ->
    "function " ^ string_of_typ t ^ " (" ^ String.concat ", " (List.map string_of_typ tl) ^ ")"
  | ArrRef(t) -> string_of_typ t ^ "[]"

and string_of_dectr = function
    DecId(s) -> s
  | DecArr(d, 0) -> string_of_dectr d ^ "[]"
  | DecArr(d, l) -> string_of_dectr d ^ "[" ^ string_of_int l ^ "]"

and string_of_bind (t, d) =
    string_of_typ t ^ " " ^ string_of_dectr d

and string_of_uop = function
    Neg -> "-"
  | Not -> "!"
  | Inc -> "++"
  | Dec -> "--"
  | UMult -> "**"
  | UDiv -> "//"
  | UPow -> "^^"

and string_of_expr = function
    IntLit(l) -> string_of_int l
  | FloatLit(f) -> string_of_float f
  | BoolLit(b) -> string_of_bool b
  | ArrLit(el) -> "[" ^ String.concat ", " (List.map string_of_expr el) ^ "]"
  | PixLit(el) -> "{" ^ String.concat ", " (List.map string_of_expr el) ^ "}"
  | Id(s) -> s
  | Binop(e1, o, e2) ->
      string_of_expr e1 ^ " " ^ string_of_op o ^ " " ^ string_of_expr e2
  | Unop(o, e) -> 
      match o with
      Neg -> string_of_uop o ^ string_of_expr e
    | Not -> string_of_uop o ^ string_of_expr e
    | Inc -> string_of_expr e ^ string_of_uop o
    | Dec -> string_of_expr e ^ string_of_uop o
    | UMult -> string_of_expr e ^ string_of_uop o
    | UDiv -> string_of_expr e ^ string_of_uop o
    | UPow -> string_of_expr e ^ string_of_uop o;
    ;
  | Assign(v, e) -> string_of_expr v ^ " = " ^ string_of_expr e
  | Call(f, el) ->
      string_of_expr f ^ "(" ^ String.concat ", " (List.map string_of_expr el) ^ ")"
  | EleAt(arr, idx) -> string_of_expr arr ^ "[" ^ string_of_expr idx ^ "]"
  | PropAcc(e, id) -> string_of_expr e ^ "." ^ id
  | AnonFunc(func) -> string_of_fdecl func
  | Noexpr -> ""

and string_of_initdectr = function
    InitDectr(s, Noexpr) -> string_of_dectr s
  | InitDectr(s, e) -> string_of_dectr s ^ " = " ^ string_of_expr e

and string_of_stmt = function
    Block(stmts) ->
      "{\n" ^ String.concat "" (List.map string_of_stmt stmts) ^ "}\n"
  | Expr(expr) -> string_of_expr expr ^ ";\n";
  | Vdef(t, dl) -> string_of_typ t ^ " " ^
      String.concat ", " (List.map string_of_initdectr (List.rev dl)) ^ ";\n";
  | Return(expr) -> "return " ^ string_of_expr expr ^ ";\n";
  | If(e, s, Block([])) -> "if (" ^ string_of_expr e ^ ")\n" ^ string_of_stmt s
  | If(e, s1, s2) ->  "if (" ^ string_of_expr e ^ ")\n" ^
      string_of_stmt s1 ^ "else\n" ^ string_of_stmt s2
  | For(e1, e2, e3, s) ->
      "for (" ^ string_of_expr e1  ^ " ; " ^ string_of_expr e2 ^ " ; " ^
      string_of_expr e3  ^ ") " ^ string_of_stmt s
  | While(e, s) -> "while (" ^ string_of_expr e ^ ") " ^ string_of_stmt s

and string_of_fdecl fdecl =
  "function " ^ string_of_typ fdecl.typ ^ " " ^
  fdecl.fname ^ "(" ^ String.concat ", " (List.map string_of_bind fdecl.formals) ^
  ")\n{\n" ^
  String.concat "" (List.map string_of_stmt fdecl.body) ^
  "}"

and string_of_funcs = function
    [] -> ""
  | funcs -> String.concat "\n\n" (List.map string_of_fdecl (List.rev funcs)) ^ "\n\n"

and string_of_program (funcs, stmts) =
  string_of_funcs funcs ^
  String.concat "" (List.map string_of_stmt (List.rev stmts))

