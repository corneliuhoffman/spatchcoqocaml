(* The first section of the grammar definition, called the *header*,
   is the part that appears below between %{ and %}.  It is code
   that will simply be copied literally into the generated parser.ml. 
   Here we use it just to open the Ast module so that, later on
   in the grammar definition, we can write expressions like 
   [Int i] instead of [Ast.Int i]. *)

%{
open Ast
%}

(* The next section of the grammar definition, called the *declarations*,
   first declares all the lexical *tokens* of the language.  These are 
   all the kinds of tokens we can expect to read from the token stream.
   Note that each of these is just a descriptive name---nothing so far
   says that LPAREN really corresponds to '(', for example.  The tokens
   that have a <type> annotation appearing in them are declaring that
   they will carry some additional data along with them.  In the
   case of INT, that's an OCaml int.  In the case of ID, that's
   an OCaml string. *)

%token <string> ID
%token SPACE
%token PLUS
%token DIV
%token MINUS
%token TIMES
%token POW
%token LPAREN
%token RPAREN

%token COMMA
%token <string> LIST
%token AND
%token OR
%token UNION
%token INTERSECTION
%token IN 
%token COMPLEMENT 
%token SETMINUS
%token EMPTYSET
%token SUCC

%token EXISTS
%token FORALL
%token NOT
%token IMPLIES
%token EQUALS
%token SUBSET

%token NE
%token IFF

%token EOF

(* After declaring the tokens, we have to provide some additional information
   about precedence and associativity.  The following declarations say that
   PLUS is left associative, that IN is not associative, and that PLUS
   has higher precedence than IN (because PLUS appears on a line after IN).  
   
   Because PLUS is left associative, "1+2+3" will parse as "(1+2)+3"
   and not as "1+(2+3)".
   
   Because PLUS has higher precedence than IN, "let x=1 in x+2" will
   parse as "let x=1 in (x+2)" and not as "(let x=1 in x)+2". *)











%nonassoc LIST
%nonassoc EMPTYSET

%left COMMA
%right below_INTERSECTION1  

%nonassoc IMPLIES
%right OR
%right AND
%nonassoc EQUALS
%nonassoc EXISTS
%nonassoc FORALL
%nonassoc NE
%nonassoc NOT
%right below_INTERSECTION  

%right  SUBSET 

%nonassoc IN
%right INTERSECTION 
%right UNION


%nonassoc COMPLEMENT 



%right DIV
%right PLUS
%right MINUS
%right SETMINUS
%right TIMES
%nonassoc SUCC
%right POW
%right IFF



(* After declaring associativity and precedence, we need to declare what
   the starting point is for parsing the language.  The following
   declaration says to start with a rule (defined below) named [prog].
   The declaration also says that parsing a [prog] will return an OCaml
   value of type [Ast.expr]. *)

%start <Ast.expr> prog

(* The following %% ends the declarations section of the grammar definition. *)

%%


prog:
  | e = expr; EOF { e }
  ;
  

          

  
expr: 
  |EMPTYSET; { EmptySet }


  

  | e=expr; POW; f=expr {Pow(e,f)}
  | SUCC; e= expr {Succ(e)}
  
  | e1= expr; IFF; e2 = expr { Iff(e1,e2) }
  | e1=expr; DIV; e2 = expr {Div(e1, e2)} 
  | e1 = expr; PLUS; e2 = expr { Add(e1,e2) }
  | e1 = expr; MINUS; e2 = expr { Minus(e1,e2) }
  | e1 = expr; SETMINUS; e2 = expr { Setminus(e1,e2) }
  | e1 = expr; TIMES; e2 = expr { Times(e1,e2) }
  | e1 = expr; AND; e2 = expr { And(e1,e2) }
  | e1 = expr; OR; e2 = expr { Or(e1,e2) }
  | e1 = expr; UNION; e2 = expr { Union(e1,e2) }
   
  | e1 = expr; INTERSECTION; e2 = expr {Intersection(e1,e2) }
  | COMPLEMENT; e=expr {Complement(e)}
  | e1=expr; SUBSET; e2=expr {Subset(e1, e2)} 
  
  | e1= expr; EQUALS; e2=expr {Equals(e1, e2)}
  | e1= expr; NE; e2=expr {Not(Equals(e1, e2))}

  | NOT; e=expr {Not(e)}
  | EXISTS; e1 = expr; COMMA; e3=expr {Exists(e1 , e3)}
  | FORALL; e1 = expr; COMMA; e3=expr {Forall(e1 , e3)}
  | e1= expr; IMPLIES; e2 = expr { Implies(e1,e2) }
  | e1 = expr; UNION; e2 = expr ; b = ID { List([Union(e1,e2); Var b]) }
  | e1 = expr; INTERSECTION; e2 = expr ; b = ID { List([Intersection(e1,e2); Var b]) }

  | e1 = expr; IN; e2 = expr { In(e1,e2) }
  | LPAREN; e = expr; RPAREN {(e)} 
  | e1= expr; IMPLIES; e2 = expr; b=ID %prec below_INTERSECTION1 { Implies(e1,(List([e2; Var b]))) }
  |func {$1};


func:
  | a =func; b = ID  %prec below_INTERSECTION { List ([ a; Var b]) }
  | x = ID { Var x }
  ;



(* And that's the end of the grammar definition. *)
