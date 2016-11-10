%{
(* L1 Compiler
 * L1 grammar
 * Author: Kaustuv Chaudhuri <kaustuv+@cs.cmu.edu>
 * Modified: Frank Pfenning <fp@cs.cmu.edu>
 *
 * Modified: Anand Subramanian <asubrama@andrew.cmu.edu> Fall 2010
 * Now conforms to the L1 fragment of C0
 *
 * Converted to OCaml by Michael Duggan <md5i@cs.cmu.edu>
 *)

module A = Ast

let ploc (left, right) =
  Parsing.rhs_start left, Parsing.rhs_end right
let mark e (left, right) =
  A.Marked (Mark.mark' (e, ParseState.ext (ploc (left, right))))
let marks e (left, right) =
  A.Markeds (Mark.mark' (e, ParseState.ext (ploc (left, right))))

(* expand_asnop (id, "op=", exp) region = "id = id op exps"
 * or = "id = exp" if asnop is "="
 * syntactically expands a compound assignment operator
 *)
let expand_asnop a b =
  match a, b with
    (id, None, exp), (left, right) ->
      A.Assign(id, exp)
  | (id, Some oper, exp), (left, right) ->
      A.Assign(id, mark (A.OpExp (oper, [A.Var(id); exp])) (left, right))

%}

%token EOF
%token SEMI
%token <Int32.t> INTCONST
%token <Symbol.symbol> IDENT
%token RETURN
%token INT
%token MAIN
%token PLUS MINUS STAR SLASH PERCENT
%token ASSIGN PLUSEQ MINUSEQ STAREQ SLASHEQ PERCENTEQ
%token LBRACE RBRACE
%token LPAREN RPAREN
%token UNARY ASNOP
/* UNARY and ASNOP are dummy terminals.
 * We need dummy terminals if we wish to assign a precedence
 * to a rule that does not correspond to the precedence of
 * the rightmost terminal in that rule.
 * Implicit in this is that precedence can only be infered
 * terminals. Therefore, don't try to assign precedence to "rules"
 */

%type <Ast.decl list * Ast.stm list> program

%left PLUS MINUS
%left STAR SLASH PERCENT
%right UNARY
%left LPAREN

%start program

%%

program :
  INT MAIN LPAREN RPAREN LBRACE decls stmts RBRACE EOF { $6, $7 }
  ;

decls :
  /* empty */                   { [] }
 | decl decls                   { $1::$2 }
 ;

decl :
  INT IDENT SEMI                { $2 }
  ;

stmts :
  /* empty */                   { [] }
 | stmt stmts                   { $1::$2 }
 ;

stmt :
  simp SEMI                     { marks $1 (1, 1) }
 | RETURN exp SEMI               { marks (A.Return $2) (1, 2) }
 ;

simp :
  IDENT asnop exp %prec ASNOP   { expand_asnop ($1, $2, $3) (1, 3) }
  ;

exp :
  LPAREN exp RPAREN             { $2 }
 | INTCONST                     { mark (A.ConstExp $1) (1, 1) }
 | IDENT                        { mark (A.Var $1) (1, 1) }
 | exp PLUS exp                 { mark (A.OpExp (A.PLUS, [$1; $3])) (1, 3) }
 | exp MINUS exp                { mark (A.OpExp (A.MINUS, [$1; $3])) (1, 3) }
 | exp STAR exp                 { mark (A.OpExp (A.TIMES, [$1; $3])) (1, 3) }
 | exp SLASH exp                { mark (A.OpExp (A.DIVIDEDBY, [$1; $3]))
                                    (1, 3) }
 | exp PERCENT exp              { mark (A.OpExp (A.MODULO, [$1; $3])) (1, 3) }
 | MINUS exp %prec UNARY        { mark (A.OpExp (A.NEGATIVE, [$2])) (1, 2) }
 ;

asnop :
  ASSIGN                        { None }
 | PLUSEQ                       { Some A.PLUS }
 | MINUSEQ                      { Some A.MINUS }
 | STAREQ                       { Some A.TIMES }
 | SLASHEQ                      { Some A.DIVIDEDBY }
 | PERCENTEQ                    { Some A.MODULO }
 ;

%%
