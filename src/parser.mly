%{
open Syntax
%}

%token LPAREN RPAREN SEMISEMI
<<<<<<< HEAD
%token PLUS MULT LT
%token IF THEN ELSE TRUE FALSE ANDAND BARBAR
%token LET IN EQ
%token RARROW FUN DFUN
%token REC
%token MATCH WITH NIL APPEND BAR
%token LBOX RBOX SEMI
=======
%token PLUS MULT LT AND OR  
%token IF THEN ELSE TRUE FALSE
%token LET IN EQ ANDLET 
%token RARROW FUN 
%token DFUN 
%token REC 

%token QUIT
>>>>>>> MLinter

%token <int> INTV
%token <Syntax.id> ID

%start toplevel
%type <Syntax.program> toplevel
%%

toplevel :
    e=Expr SEMISEMI { Exp e }
<<<<<<< HEAD
  | LET x=ID EQ e=Expr SEMISEMI { Decl (x, e) }
  // カリー化されたlet宣言の推論規則です。
  | LET x=ID e=LetParaExpr SEMISEMI { Decl (x, e) }
  // let rec宣言の推論規則です。
  | LET REC x1=ID EQ FUN x2=ID RARROW e=Expr SEMISEMI { RecDecl (x1, x2, e) }

Expr :
    e=OrExpr { e }

OrExpr :
  // ||の推論規則です。最も結合が弱く、また左結合であるためこのようになっています。
    l=OrExpr BARBAR r=AndExpr { BinOp (Barbar, l, r) }
  | e=AndExpr { e }

AndExpr :
  // &&の推論規則です。||の次に結合が弱く、また左結合であるためこのようになっています。
    l=AndExpr ANDAND r=LTExpr { BinOp (Andand, l, r) }
  | e=LTExpr { e }

LTExpr :
    l=PExpr LT r=PExpr { BinOp (Lt, l, r) }
  | e=PExpr { e }

PExpr :
    l=PExpr PLUS r=MExpr { BinOp (Plus, l, r) }
  | e=MExpr { e }

MExpr :
    l=MExpr MULT r=SyntacticExpr { BinOp (Mult, l, r) }
  | e=SyntacticExpr { e }

SyntacticExpr :
    e=IfExpr { e }
  // カリー化されたlet式、fun式はこの推論規則で導出されます。ただし、カリー化されたlet式は解釈時にfun式の解釈を利用しますので通常のlet式とは区別しています。
  | e=LetCurriedExpr { e }
  | e=LetExpr { e }
  | e=FunCurriedExpr { e }
  | e=DFunExpr { e }
  | e=MatchExpr { e }
  | e=ConsExpr { e }
=======
  //| LET x=ID EQ e=Expr SEMISEMI { Decl (x,e)} 
  | LET e_ls=AndLetExpr SEMISEMI {Decl e_ls} 
  | LET REC x1=ID EQ FUN x2=ID RARROW e=Expr SEMISEMI { RecDecl (x1, x2, e)}
  | QUIT SEMISEMI {QuitDecl}

Expr :
    e=IfExpr { e }
  | e=ORExpr { e }  
  | e=LetExpr { e } 
  | e=FunExpr { e } 
  | e=DFunExpr { e } 
  | e=LetRecExpr { e } 
    //| e=LTExpr { e }


LetExpr :
  LET e_ls=AndLetExpr IN e2=Expr { LetExp (e_ls, e2) } 
  

AndLetExpr :
   x=ID EQ e=Expr { [(x,e)] }
  | x=ID EQ e1=Expr ANDLET e2=AndLetExpr { (x,e1) :: e2 } 
  | x=ID args=MultiArgs EQ e=Expr { [(x, argstoFun args e )] } 
  | x=ID args=MultiArgs EQ e=Expr ANDLET e2=AndLetExpr { (x, argstoFun args e ) :: e2 } 
  

>>>>>>> MLinter

MultiArgs :
  x=ID { [x] }
| x=ID e=MultiArgs { x :: e }

<<<<<<< HEAD
// カリー化されたlet式の推論規則です。元々はfunが連続して並ぶよう記述されるので、それと同様に引数は右結合になっています。
LetCurriedExpr :
    LET x=ID e1=LetParaExpr IN e2=Expr { LetExp (x, e1, e2) }

LetParaExpr :
    x=ID e=LetParaExpr { FunExp (x, e) }
  | x=ID EQ e=Expr { FunExp (x, e) }

LetExpr :
    LET x=ID EQ e1=Expr IN e2=Expr { LetExp (x, e1, e2) }
    // let rec式の推論規則です。
  | LET REC x1=ID EQ FUN x2=ID RARROW e1=Expr IN e2=Expr { LetRecExp (x1, x2, e1, e2) }

// カリー化されたfun式の推論規則です。元々の記法に合わせて引数は右結合にしています。
FunCurriedExpr :
    FUN e=FunParaExpr { e }

FunParaExpr :
    x=ID e=FunParaExpr { FunExp (x, e) }
  | x=ID RARROW e=Expr { FunExp (x, e) }

DFunExpr :
    DFUN x=ID RARROW e=Expr { DFunExp (x, e) }
=======
FunExpr : 
   FUN e=FunMultiAgrsExpr { e } 
  

DFunExpr :
   DFUN x=ID RARROW e=Expr { DFunExp (x,e)} 

FunMultiAgrsExpr : 
   x=ID RARROW e=Expr { FunExp (x,e) }
  | x=ID e=FunMultiAgrsExpr { FunExp (x,e) }

LetRecExpr :
   LET REC x1=ID EQ FUN x2=ID RARROW e1=Expr IN e2=Expr { LetRecExp (x1,x2,e1,e2)} 

ORExpr :  
 l=ANDExpr OR r=ANDExpr { BinOp (Or, l, r) }
| e=ANDExpr { e }

ANDExpr :  
 l=LTExpr AND r=LTExpr { BinOp (And, l, r) }
| e=LTExpr { e }


LTExpr :
    l=PExpr LT r=PExpr { BinOp (Lt, l, r) }
  | e=PExpr { e }
>>>>>>> MLinter

MatchExpr :
    MATCH e1=Expr WITH NIL RARROW e2=Expr BAR x1=ID APPEND x2=ID RARROW e3=Expr { MatchExp (e1, e2, x1, x2, e3) }

<<<<<<< HEAD
ConsExpr :
    i=AppExpr APPEND e=ConsExpr { ConsExp (i, e) }
  | e=AppExpr { e }

=======
MExpr :
    l=MExpr MULT r=AppExpr { BinOp (Mult, l, r) } 
  | e=AppExpr { e } 
  
>>>>>>> MLinter
AppExpr :
    e1=AppExpr e2=ListStartExpr { AppExp (e1, e2) }
  | e=ListStartExpr { e } 

ListStartExpr :
    LBOX e=ListExpr { e }
  | e=AExpr { e }
<<<<<<< HEAD

ListExpr :
    e1=ListStartExpr SEMI e2=ListExpr { ConsExp (e1, e2) }
  | e=ListStartExpr RBOX { ConsExp (e, NilExp) }

=======
  

InfixExpr :
    LPAREN PLUS RPAREN { FunExp ("x", FunExp ("y", BinOp (Plus, Var "x", Var "y")))}
  | LPAREN MULT RPAREN { FunExp ("x", FunExp ("y", BinOp (Mult, Var "x", Var "y")))}
  | LPAREN LT RPAREN { FunExp ("x", FunExp ("y", BinOp (Lt, Var "x", Var "y")))}
  | LPAREN OR RPAREN { FunExp ("x", FunExp ("y", BinOp (Or, Var "x", Var "y"))) }
  | LPAREN AND RPAREN { FunExp ("x", FunExp ("y", BinOp (And, Var "x", Var "y"))) }
  
    
>>>>>>> MLinter
AExpr :
    i=INTV { ILit i }
  | TRUE   { BLit true }
  | FALSE  { BLit false }
  | i=ID   { Var i }
  | LPAREN e=Expr RPAREN { e }
<<<<<<< HEAD
  | NIL { NilExp }
=======
  | e=InfixExpr { e }

// AExprListExpr : 
//    e=AExpr rest=AExprListExpr { e :: rest }
//   | e=AExpr { [e] }
  

IfExpr :
    IF c=Expr THEN t=Expr ELSE e=Expr { IfExp (c, t, e) }
>>>>>>> MLinter
