%{
  open Absyn
%}

%token EOF
%token <string> ID
%token <int> INT
%token <string> STRING
%token COMMA COLON SEMICOLON LPAREN RPAREN LBRACK RBRACK
%token LBRACE RBRACE DOT
%token PLUS MINUS TIMES DIVIDE EQ NEQ LT LE GT GE
%token AND OR ASSIGN
%token ARRAY IF THEN ELSE WHILE FOR TO DO LET IN END OF
%token BREAK NIL
%token FUNCTION VAR TYPE

%start program
%type <Absyn.exp> program

%nonassoc THEN
%nonassoc ELSE
%nonassoc LPAREN

%right OF
%nonassoc DO

%left SEMICOLON
%left ASSIGN
%left OR
%left AND
%nonassoc EQ NEQ LT LE GT GE
%left PLUS MINUS
%left TIMES DIVIDE

%nonassoc UMINUS

%%

program:
  | exp EOF { $1 }

exp:
  | NIL 
  { NilExp }
  | LPAREN expseq RPAREN 
  { SeqExp($2) }
  | ID LPAREN funcexpseq RPAREN 
  { CallExp {func=Symbol.symbol $1; 
            args = $3;
            pos = $startpos}}
  | ID LPAREN RPAREN 
  { 
    CallExp {func = Symbol.symbol $1;
            args = [];
            pos = $startpos}
  }
  | arith { 
    $1
  }
  | comp { 
    $1
  }
  | bool { 
    $1
  }
  | ID LBRACE idexpseq RBRACE { 
    RecordExp {fields = $3; typ = Symbol.symbol $1; pos = $startpos}
  }
  | ID LBRACE RBRACE { 
    RecordExp {fields = []; typ = Symbol.symbol $1; pos = $startpos}
  }
  | ID LBRACK exp RBRACK OF exp { 
      ArrayExp {typ = Symbol.symbol $1; 
      size = $3;
      init = $6;
      pos = $startpos}
  }
  | IF exp THEN exp ELSE exp { 
    IfExp {
      test = $2;
      then_ = $4;
      else_ = Some $6;
      pos = $startpos
    }
  }
  | IF exp THEN exp { 
    IfExp {
      test = $2;
      then_ = $4;
      else_ = None;
      pos = $startpos
    }
  }
  | WHILE exp DO exp { 
    WhileExp {
      test = $2;
      body = $4;
      pos = $startpos
    }
  }
  | FOR ID ASSIGN exp TO exp DO exp { 
    ForExp {
      var = Symbol.symbol $2;
      escape = ref true;
      lo = $4;
      hi = $6;
      body = $8;
      pos = $startpos
    }
  }
  | BREAK { 
    BreakExp ($startpos)
  }
  | LET decs IN expseq END { 
    LetExp {
      decs = $2;
      body = SeqExp($4);
      pos = $startpos
    }
  } 
  | INT { 
    IntExp ($1)
  }
  | STRING { 
    StringExp ($1, $startpos)
  }
  | lvalue { 
    VarExp($1)
  }
  | stmt { 
    $1
  }
  | LPAREN RPAREN { 
    NilExp
  }
stmt:

  | lvalue ASSIGN exp { 
    AssignExp {
      var = $1;
      exp = $3;
      pos = $startpos;
    }
  }
funcexpseq:
  | exp { 
    [$1]
  }
  | exp COMMA funcexpseq { $1 :: $3 }
idexpseq: 
  | ID EQ exp { 
    [(Symbol.symbol $1, $3, $startpos)]
  }
  | ID EQ exp COMMA idexpseq { 
    (Symbol.symbol $1, $3, $startpos) :: $5
  }
arith:
  | exp PLUS exp { 
    OpExp {
      left = $1;
      oper = PlusOp;
      right = $3;
      pos = $startpos;
    }
  }
  | exp MINUS exp { 
    OpExp {
      left = $1;
      oper = MinusOp;
      right = $3;
      pos = $startpos
    }
  }
  | exp TIMES exp {   
    OpExp {
      left = $1;
      oper = TimesOp;
      right = $3;
      pos = $startpos
    }
  }
  | exp DIVIDE exp { 
    OpExp {
      left = $1;
      oper = DivideOp;
      right = $3;
      pos = $startpos
    }
  }
  | MINUS exp  %prec UMINUS { 
    OpExp {
      left = IntExp (0);
      oper = MinusOp;
      right = $2;
      pos = $startpos
    }
  }

comp:
  | exp EQ exp { 
    OpExp {
      left = $1;
      oper = EqOp;
      right = $3;
      pos = $startpos
    }
  }
  | exp LE exp { 
    OpExp {
    left = $1;
    oper = LeOp;
    right = $3;
    pos = $startpos
    }
  }
  | exp GE exp { 
        OpExp {
    left = $1;
    oper = GeOp;
    right = $3;
    pos = $startpos }
  }
  | exp GT exp { 
    OpExp {
    left = $1;
    oper = GtOp;
    right = $3;
    pos = $startpos
    }
  }
  | exp LT exp { 
    OpExp {
    left = $1;
    oper = LtOp;
    right = $3;
    pos = $startpos
    }
  }
  | exp NEQ exp { 
    OpExp {
    left = $1;
    oper = NeqOp;
    right = $3;
    pos = $startpos
    }
  }
bool:
  | exp AND exp { 
    IfExp
    {
      test = $1;
      then_ = $3;
      else_ = Some (IntExp 0);
      pos = $startpos
    }
  }
  | exp OR exp { 
    IfExp
    {
      test = $1;
      then_ = (IntExp 1);
      else_ = Some ($3);
      pos = $startpos
    }
  }
expseq:
  | exp { 
    [($1, $startpos)]
  }
  | exp SEMICOLON expseq { 
    ($1, $startpos) :: $3
  }
decs :
  | tydecs not_tydecs { TypeDec($1) :: $2}
  | vardec decs { $1 :: $2}
  | fundecs not_fundecs { FunctionDec($1) :: $2}
  | { [] }
not_fundecs:
  | tydecs not_tydecs { TypeDec($1) :: $2}
  | vardec decs { $1 :: $2}
  | { [] }
not_tydecs:
  | fundecs not_fundecs {FunctionDec($1) :: $2}
  | vardec decs { $1 :: $2 }
  | { [] }
dec :
  | tydecs { TypeDec($1) }
  | vardec { $1 }
  | fundecs { FunctionDec($1) }
tydecs:
  | tydec tydecs { 
    $1 :: $2
  }
  | tydec { [$1] }
tydec :
  | TYPE type_id EQ ty { 
    {name= $2; ty = $4; pos = $startpos}
  }
ty :
  | type_id { 
    NameTy ($1 , $startpos)
  }
  | LBRACE tyfields RBRACE { 
    RecordTy ($2)
  }
  | ARRAY OF type_id { 
    ArrayTy ($3 , $startpos)
  }

tyfields :
  | ID COLON type_id { 
    [{name = Symbol.symbol $1; 
    escape = ref true; 
    typ = $3; 
    pos = $startpos
    }]
  }
  | ID COLON type_id COMMA tyfields { 
    {name = Symbol.symbol $1; 
    escape = ref true; 
    typ = $3; 
    pos = $startpos
    } :: $5
  }
  | { [] }

vardec :
  | VAR ID ASSIGN exp { 
    VarDec {
      name = Symbol.symbol $2;
      escape = ref true;
      typ = None;
      init = $4;
      pos = $startpos
    }
  }
  | VAR ID COLON type_id ASSIGN exp { 
    VarDec {
      name = Symbol.symbol $2;
      escape = ref true;
      typ = Some ($4, $startpos($4));
      init = $6;
      pos = $startpos
    }
  }
fundecs:
| fundec fundecs { $1 :: $2}
| fundec { [$1] }
fundec : 
  | FUNCTION ID LPAREN tyfields RPAREN EQ exp { 
    {name = Symbol.symbol $2;
    params = $4;
    result = None;
    body = $7;
    pos = $startpos
    }
  }
  | FUNCTION ID LPAREN tyfields RPAREN COLON type_id EQ exp { 
    {
      name = Symbol.symbol $2;
      params = $4;
      result = Some ($7, $startpos($7));
      body = $9;
      pos = $startpos
    }
  }

lvalue :
  | ID { SimpleVar (Symbol.symbol $1, $startpos)}
  | lvalue DOT ID { FieldVar($1, Symbol.symbol $3, $startpos)}
  | lvalue LBRACK exp RBRACK { 
    SubscriptVar ($1 , $3 , $startpos)
  }
  | ID LBRACK exp RBRACK { 
    SubscriptVar(
      SimpleVar(Symbol.symbol $1, $startpos),
      $3 ,
      $startpos
    )
  }
type_id :
  | ID { Symbol.symbol $1 }
%%