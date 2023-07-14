%{
open Syntax
%}

%token LPAREN RPAREN SEMISEMI
%token PLUS MULT LT GT
%token IF THEN ELSE TRUE FALSE
%token LOGICAND LOGICOR
%token LET IN EQ AND LETRECURSIVE

%token <int> INTV
%token <Syntax.id> ID

%start toplevel
%type <Syntax.program> toplevel
%%

toplevel :
    e=Expr SEMISEMI { Exp e }
  | LET x=ID EQ e=Expr SEMISEMI { Decl (x, e) }
  | LET decls=LetDecls SEMISEMI { LetDecls decls } 
  | LET x=ID EQ e1=Expr LETRECURSIVE decls=LetRecDecls SEMISEMI { LetRecDecl ((x, e1)::decls) }

Expr :
    e=IfExpr  { e }
  | e=LetExpr { e }
  | e=LetRecExpr { e } 
  | e=OrExpr  { e }

LetExpr :
     LET x=ID EQ e1=Expr IN e2=Expr { LetExp (x, e1, e2) }
     
LetDecls :
     x=ID EQ e=Expr { [(x, e)] }
   | x=ID EQ e=Expr LET decls=LetDecls { (x, e) :: decls }
   | x=ID EQ e=Expr AND decls=LetDecls { (x, e) :: decls }

LetAndExpr :
  | LET bindings=LetAndBindings IN e=Expr { LetAndExp (bindings, e) }

LetAndBindings :
  | LET x=ID EQ e=Expr { [(x, e)] }
  | bindings=LetAndBindings AND x=ID EQ e=Expr { (x, e) :: bindings }

Decl :
  | LET bindings=LetAndBindings { LetAndDecl bindings }

LetRecExpr :
  LET x=ID EQ e1=Expr LETRECURSIVE decls=LetRecDecls IN e2=Expr { LetRecExp ((x, e1)::decls, e2) }

LetRecDecls :
  x=ID EQ e=Expr AND decls=LetRecDecls { (x, e) :: decls }
  | x=ID EQ e=Expr { [(x, e)] }

OrExpr :
    l=AndExpr LOGICOR r=OrExpr  { LogicOp (Or, l, r) }
  | e=AndExpr { e }

AndExpr :
    l=CompExpr LOGICAND r=AndExpr { LogicOp (And, l, r) }
  | e=CompExpr { e }

CompExpr :
    l=AddExpr LT r=AddExpr { BinOp (Lt, l, r) }
  | l=AddExpr GT r=AddExpr { BinOp (Gt, l, r) }
  | e=AddExpr { e }

AddExpr :
    l=AddExpr PLUS r=MultExpr { BinOp (Plus, l, r) }
  | e=MultExpr { e }

MultExpr :
    l=MultExpr MULT r=Atom { BinOp (Mult, l, r) }
  | e=Atom { e }

Atom :
    i=INTV { ILit i }
  | TRUE   { BLit true }
  | FALSE  { BLit false }
  | i=ID   { Var i }
  | LPAREN e=Expr RPAREN { e }

IfExpr :
    IF c=Expr THEN t=Expr ELSE e=Expr { IfExp (c, t, e) }
