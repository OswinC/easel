(* Ocamllex scanner for easel *)

{ open Parser }

let digit = ['0'-'9']
let exp = 'e' ['+' '-']? digit+

rule token = parse
  [' ' '\t' '\r' '\n'] { token lexbuf } (* Whitespace *)
| "/*"     { comment 1 lexbuf }           (* Comments *)
| '('      { LPAREN }
| ')'      { RPAREN }
| '{'      { LBRACE }
| '}'      { RBRACE }
| ';'      { SEMI }
| ','      { COMMA }
| '+'      { PLUS }
| '-'      { MINUS }
| '*'      { TIMES }
| '/'      { DIVIDE }
| '='      { ASSIGN }
| "=="     { EQ }
| "!="     { NEQ }
| '<'      { LT }
| "<="     { LEQ }
| ">"      { GT }
| ">="     { GEQ }
| "&&"     { AND }
| "||"     { OR }
| "!"      { NOT }
| "if"     { IF }
| "else"   { ELSE }
| "for"    { FOR }
| "while"  { WHILE }
| "return" { RETURN }
| "int"    { INT }
| "float"  { FLOAT }
| "bool"   { BOOL }
| "void"   { VOID }
| "pix"    { PIX }
| "function"  { FUNC }
| "true" | "false" as lxm { BOOLLIT(bool_of_string lxm) }
| digit+ as lxm { INTLIT(int_of_string lxm) }
| (digit* '.' digit+ | digit+ '.') exp? | digit+ exp as lxm
     { FLOATLIT(float_of_string lxm) }
| ['a'-'z' 'A'-'Z' '_']['a'-'z' 'A'-'Z' '0'-'9' '_']* as lxm { ID(lxm) }
| eof { EOF }
| _ as char { raise (Failure("illegal character " ^ Char.escaped char)) }

and comment cnt = parse
  "/*" { comment (cnt + 1) lexbuf }
|  "*/" { if cnt = 1 then token lexbuf else comment (cnt - 1) lexbuf }
| _    { comment cnt lexbuf }
