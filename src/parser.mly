%{
open Syntax
%}

%token LPAREN RPAREN SEMISEMI
%token PLUS MULT LT
%token IF THEN ELSE TRUE FALSE
%token AND OR
%token LET IN ASSIGN LITAND
%token RARROW FUN
%token REC
%token NIL COLONCOLON LBRACKET RBRACKET SEMI
%token MATCH WITH BAR

%token <int> INTV
%token <Syntax.id> ID

%start toplevel
%type <Syntax.program list> toplevel
%%

toplevel :
    e=Expr SEMISEMI { [ Exp e ] }
  | e=DeclExpr SEMISEMI { e }
  | e=RecDeclExpr SEMISEMI { [ e ] }

DeclExpr :
    LET e=DeclAssignExpr { [ Decl e ] }
  | LET e=DeclAssignExpr r=DeclExpr { Decl e :: r }

DeclAssignExpr :
    x=ID ASSIGN e1=Expr { [ (x, e1) ] }
  | x=ID ASSIGN e1=Expr LITAND r=DeclAssignExpr { (x, e1) :: r }

RecDeclExpr :
    LET REC x=ID ASSIGN FUN a=ID RARROW e=Expr { RecDecl (x, a, e) }

Expr :
    e=LetExpr { e }
  | e=RecLetExpr { e }
  | e=IfExpr { e }
  | e=ORExpr { e }
  | e=FunExpr { e }
  | e=MatchExpr { e }

FunExpr :
    FUN x=ID RARROW e=Expr { FunExp (x, e) }

LetExpr :
    LET l=LetAssignExpr IN e2=Expr { LetExp (l, e2) }

LetAssignExpr :
    x=ID ASSIGN e1=Expr LITAND r=LetAssignExpr { (x, e1) :: r }
  | x=ID ASSIGN e1=Expr { [ (x, e1) ] }

RecLetExpr :
    LET REC x=ID ASSIGN FUN a=ID RARROW e1=Expr IN e2=Expr { LetRecExp (x, a, e1, e2) }

IfExpr :
    IF c=Expr THEN t=Expr ELSE e=Expr { IfExp (c, t, e) }

MatchExpr :
    MATCH e=Expr WITH NIL RARROW e1=Expr BAR x=ID COLONCOLON xs=ID RARROW e2=Expr { MatchExp (e, e1, x, xs, e2) }

ORExpr :
    l=ORExpr OR r=ANDExpr { BinOp (Or, l, r) }
  | e=ANDExpr { e }

ANDExpr :
    l=ANDExpr AND r=LTExpr { BinOp (And, l, r) }
  | e=LTExpr { e }

LTExpr :
    l=LTExpr LT r=AppendExpr { BinOp (Lt, l, r) }
  | e=AppendExpr { e }

AppendExpr :
    l=ListExpr COLONCOLON r=AppendExpr { BinOp (Append, l, r) }
  | e=ListExpr { e }

ListExpr :
    LBRACKET e=ListInternalExpr RBRACKET { e }
  | LBRACKET RBRACKET { Nil }
  | e=PExpr { e }

ListInternalExpr :
    l=ListExpr SEMI r=ListInternalExpr { BinOp (Append, l, r) }
  | e=ListExpr { BinOp (Append, e, Nil) }

PExpr :
    l=PExpr PLUS r=MExpr { BinOp (Plus, l, r) }
  | e=MExpr { e }

MExpr :
    l=MExpr MULT r=AppExpr { BinOp (Mult, l, r) }
  | e=AppExpr { e }

AppExpr :
    e1=AppExpr e2=AExpr { AppExp (e1, e2) }
  | e=AExpr { e }

AExpr :
    i=INTV { ILit i }
  | TRUE   { BLit true }
  | FALSE  { BLit false }
  | i=ID   { Var i }
  | NIL { Nil }
  | LPAREN e=Expr RPAREN { e }
