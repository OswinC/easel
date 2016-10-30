/* Ocamlyacc parser for easel */

%{
open Ast
%}

%token FUNC
%token SEMI LPAREN RPAREN LBRCK RBRCK LBRACE RBRACE COMMA
%token PLUS MINUS TIMES DIVIDE POW ASSIGN NOT
%token INC DEC UMULT UDIV UPOW 
%token EQ NEQ LT LEQ GT GEQ AND OR
%token RETURN IF ELSE FOR WHILE INT FLOAT BOOL VOID PIX
%token <int> INTLIT
%token <float> FLOATLIT
%token <bool> BOOLLIT
%token <string> ID
%token EOF

%nonassoc NOELSE
%nonassoc ELSE
%right ASSIGN
%left OR
%left AND
%left EQ NEQ
%left LT GT LEQ GEQ
%left PLUS MINUS
%left TIMES DIVIDE
%right NOT NEG

%start program
%type <Ast.program> program

%%

program:
  decls EOF { $1 }

decls:
    /* nothing */ { [], [] }
 | decls fdecl { let (fds, sts) = $1 in ($2 :: fds), sts }
 | decls stmt { let (fds, sts) = $1 in fds, ($2 :: sts) }

fdecl:
    FUNC atyp ID LPAREN formals RPAREN LBRACE stmt_list RBRACE
     { { typ = $2;
	 fname = $3;
	 formals = $5;
	 body = List.rev $8 } }

  | FUNC atyp ID LPAREN RPAREN LBRACE stmt_list RBRACE
     { { typ = $2;
	 fname = $3;
     formals = [];
	 body = List.rev $7 } }

formals:
    typ dectr                   { [($1, $2)] }
  | formals COMMA typ dectr { ($3, $4) :: $1 }

aformals:
    atyp                    { [$1] }
  | aformals COMMA atyp { $3 :: $1 }

/* type keywords that can be immediately followed by []s */
atyp:
    typ { $1 }
  | atyp LBRCK RBRCK { Arr($1) }

typ:
    prim_typ { $1 }
  | afunc_typ { $1 }

prim_typ:
    INT { Int }
  | FLOAT { Float }
  | BOOL { Bool }
  | VOID { Void }
  | PIX { Pix }

afunc_typ:
    FUNC atyp LPAREN aformals RPAREN
    { Func($2, $4) }
  | FUNC atyp LPAREN RPAREN
    { Func($2, []) }

init_dectr_list:
    init_dectr    { [$1] }
  | init_dectr_list COMMA init_dectr { $3 :: $1 }

init_dectr:
    dectr { InitDectr($1, Noexpr) }
  | dectr ASSIGN expr { InitDectr($1, $3) }

dectr:
    ID { DecId($1) }
  | dectr LBRCK INTLIT RBRCK { DecArr($1, $3) }
  | dectr LBRCK RBRCK { DecArr($1, 0) }

stmt_list:
    /* nothing */  { [] }
  | stmt_list stmt { $2 :: $1 }

stmt:
    expr SEMI { Expr $1 }
  | typ init_dectr_list SEMI { Vdef($1, $2) }
  | RETURN SEMI { Return Noexpr }
  | RETURN expr SEMI { Return $2 }
  | LBRACE stmt_list RBRACE { Block(List.rev $2) }
  | IF LPAREN expr RPAREN stmt %prec NOELSE { If($3, $5, Block([])) }
  | IF LPAREN expr RPAREN stmt ELSE stmt    { If($3, $5, $7) }
  | FOR LPAREN expr_opt SEMI expr SEMI expr_opt RPAREN stmt
     { For($3, $5, $7, $9) }
  | WHILE LPAREN expr RPAREN stmt { While($3, $5) }

anonfunc:
    FUNC atyp LPAREN formals RPAREN LBRACE stmt_list RBRACE
    { AnonFunc({ typ = $2; fname = ""; formals = $4; body = List.rev $7 }) }
  | FUNC atyp LPAREN RPAREN LBRACE stmt_list RBRACE
    { AnonFunc({ typ = $2; fname = ""; formals = []; body = List.rev $6 }) }

expr_opt:
    /* nothing */ { Noexpr }
  | expr          { $1 }

expr:
    assign_expr { $1 }

assign_expr:
    logic_or_expr                     { $1 }
  | anonfunc                          { $1 }
  | postfix_expr ASSIGN logic_or_expr { Assign($1, $3) }
  | postfix_expr ASSIGN anonfunc      { Assign($1, $3) }

logic_or_expr:
    logic_and_expr { $1 }
  | logic_or_expr OR logic_and_expr { Binop($1, Or, $3) }

logic_and_expr:
    eq_expr { $1 }
  | logic_and_expr AND eq_expr { Binop($1, And, $3) }

eq_expr:
    rel_expr { $1 }
  | eq_expr EQ rel_expr { Binop($1, Equal, $3) }
  | eq_expr NEQ rel_expr { Binop($1, Neq, $3) }

rel_expr:
    add_expr { $1 }
  | rel_expr LT add_expr { Binop($1, Less, $3) }
  | rel_expr GT add_expr { Binop($1, Greater, $3) }
  | rel_expr LEQ add_expr { Binop($1, Leq, $3) }
  | rel_expr GEQ add_expr { Binop($1, Geq, $3) }

add_expr:
    mult_expr { $1 }
  | add_expr PLUS mult_expr { Binop($1, Add, $3) }
  | add_expr MINUS mult_expr { Binop($1, Sub, $3) }

mult_expr:
    exp_expr { $1 }
  | mult_expr TIMES exp_expr { Binop($1, Mult, $3) }
  | mult_expr DIVIDE exp_expr { Binop($1, Div, $3) }

exp_expr:
    unary_expr { $1 }
  /* TODO: Should be right-associative? */
  | exp_expr POW unary_expr { Binop($1, Pow, $3) }

unary_expr:
    postfix_expr { $1 }
  | PLUS unary_expr { $2 }
  | MINUS unary_expr { Unop(Neg, $2) }
  | NOT unary_expr { Unop(Not, $2) }

postfix_expr:
    base_expr { $1 }
  | postfix_expr LBRCK expr RBRCK { EleAt($1, $3) }
  | postfix_expr LPAREN actuals_opt RPAREN { Call($1, $3) }
  | postfix_expr INC { Unop(Inc, $1) }
  | postfix_expr DEC { Unop(Dec, $1) }
  | postfix_expr UMULT { Unop(UMult, $1) }
  | postfix_expr UDIV { Unop(UDiv, $1) }
  | postfix_expr UPOW { Unop(UPow, $1) }

base_expr:
    INTLIT             { IntLit($1) }
  | FLOATLIT           { FloatLit($1) }
  | BOOLLIT            { BoolLit($1) }
  | ID                 { Id($1) }
  | LPAREN expr RPAREN { $2 }

actuals_opt:
    /* nothing */ { [] }
  | actuals_list  { List.rev $1 }

actuals_list:
    expr                    { [$1] }
  | actuals_list COMMA expr { $3 :: $1 }
