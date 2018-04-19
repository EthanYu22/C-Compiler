%{

#include "astree.h"
#include "lyutils.h"

%}

%debug
%defines
%error-verbose
%token-table
%verbose

%token TOK_CONSTANT TOK_VAR TOK_rec1 TOK_CALL TOK_ALLOCATOR 
%token TOK_UNOP TOK_BINOP TOK_EXPR TOK_RET
%token TOK_IFELSE TOK_WHILE TOK_VARDECL 
%token TOK_STATEMENT TOK_blockrec TOK_BLOCK
%token TOK_FUNC TOK_FUNCTION TOK_BASETYPE TOK_TYPE TOK_DECL
%token TOK_rec TOK_STRUCTDEF TOK_Prog TOK_ROOT

%token TOK_VOID TOK_BOOL TOK_CHAR TOK_INT TOK_STRING
%token TOK_RETURN TOK_STRUCT
%token TOK_FALSE TOK_TRUE TOK_NULL
%token TOK_IDENT TOK_INTCON TOK_CHARCON TOK_STRINGCON

%token TOK_NEWARRAY
    
%nonassoc ')'    
%right TOK_IF TOK_ELSE
%right '='    
%left  TOK_EQ TOK_NE '<' TOK_LE '>' TOK_GE
%left  '+' '-'
%left  '*' '/' '%'
%right TOK_POS TOK_NEG '!' TOK_ORD TOK_CHR
%right  '[' '.'
%left  TOK_CALL
%right  TOK_ARRAY TOK_FIELD
%nonassoc TOK_NEW
%nonassoc TOK_PAREN
    
%start parse

%%

parse       : Prog           {$$ = adopt(adopt1(new_parseroot(), $1),$1);}
            |                {$$ = new_parseroot();}
            ;	
Prog        : structdef Prog {$$ = adopt(adopt2(new_astree(TOK_Prog, $1->filenr,$1->linenr, $1->offset, "Program"), $1, $2), $2);}
            | function Prog  {$$ = adopt(adopt2(new_astree(TOK_Prog, $1->filenr,$1->linenr, $1->offset, "Program"), $1, $2), $2);}
            | statement Prog {$$ = adopt(adopt(adopt2(new_astree(TOK_Prog, $1->filenr,$1->linenr, $1->offset, "Program"), $1, $2), $1), $2);}
            | structdef      {$$ = adopt1(new_astree(TOK_Prog, $1->filenr,$1->linenr, $1->offset, "Program"), $1);}
            | function       {$$ = adopt1(new_astree(TOK_Prog, $1->filenr,$1->linenr, $1->offset, "Program"), $1);}
            | statement      {$$ = adopt(adopt1(new_astree(TOK_Prog, $1->filenr,$1->linenr, $1->offset, "Program"), $1), $1);}
            ;
            
structdef   : TOK_STRUCT TOK_IDENT '{' rec '}' {$$ = adopt(adopt1(adopt1(new_astree(TOK_STRUCTDEF, $1->filenr,$1->linenr, $1->offset, "structdef"), $2), $4), $4);}
            | TOK_STRUCT TOK_IDENT '{' '}'     {$$ = adopt1(new_astree(TOK_STRUCTDEF, $1->filenr,$1->linenr, $1->offset, "structdef"), $2);}
            ; 

rec         : decl ';' rec {$$ = adopt(adopt2(new_astree(TOK_rec, $1->filenr,$1->linenr, $1->offset, "rec_TOK"), $1, $3), $3);}
            | decl ';'     {$$ = adopt1(new_astree(TOK_rec, $1->filenr,$1->linenr, $1->offset, "rec_TOK"), $1);}
            ;
            
decl        : type TOK_IDENT {$$ = adopt2(new_astree(TOK_DECL, $1->filenr,$1->linenr, $1->offset, "decl"), $1, $2);}
            ;
            
type        : basetype              {$$ = adopt1(new_astree(TOK_TYPE, $1->filenr,$1->linenr, $1->offset, "type"), $1);}
            | basetype TOK_NEWARRAY {$$ = adopt2(new_astree(TOK_TYPE, $1->filenr,$1->linenr, $1->offset, "type"), $1, $2);}
            ; 
            
basetype    : TOK_VOID  {$$ = adopt1(new_astree(TOK_BASETYPE, $1->filenr,$1->linenr, $1->offset, "basetype"), $1);}
            | TOK_BOOL  {$$ = adopt1(new_astree(TOK_BASETYPE, $1->filenr,$1->linenr, $1->offset, "basetype"), $1);}
            | TOK_CHAR  {$$ = adopt1(new_astree(TOK_BASETYPE, $1->filenr,$1->linenr, $1->offset, "basetype"), $1);}
            | TOK_INT   {$$ = adopt1(new_astree(TOK_BASETYPE, $1->filenr,$1->linenr, $1->offset, "basetype"), $1);}
            | TOK_STRING{$$ = adopt1(new_astree(TOK_BASETYPE, $1->filenr,$1->linenr, $1->offset, "basetype"), $1);}
            | TOK_IDENT {$$ = adopt1(new_astree(TOK_BASETYPE, $1->filenr,$1->linenr, $1->offset, "basetype"), $1);}
            ;
            
function    : type TOK_IDENT '(' FUNC ')' block {$$ = adopt1(adopt(adopt1(adopt2(new_astree(TOK_FUNCTION,
                                                      $1->filenr,$1->linenr, $1->offset, "function"), $1, $2), $4), $4),$6);}
            | type TOK_IDENT '(' ')' block      {$$ = adopt1(adopt2(new_astree(TOK_FUNCTION,
                                                      $1->filenr,$1->linenr, $1->offset, "function"), $1, $2), $5);}
            ;
            
FUNC        : decl          {$$ = adopt1(new_astree(TOK_FUNC, $1->filenr,$1->linenr, $1->offset, "func"), $1);}
            | FUNC ',' decl {$$ = adopt(adopt2(new_astree(TOK_FUNC, $1->filenr, $1->linenr, $1->offset, "func"), $1, $3), $1);}
            ;
            
block       : '{' blockrec '}' {$$ = adopt(adopt1(new_astree(TOK_BLOCK, $1->filenr,$1->linenr, $1->offset, "block"), $2), $2);}
            | '{' '}'          {$$ = adopt2(new_astree(TOK_BLOCK, $1->filenr,$1->linenr, $1->offset, "block"),$1,$2);}
            | ';'              {$$ = adopt1(new_astree(TOK_BLOCK, $1->filenr,$1->linenr, $1->offset, "block"), $1);} 
            ;
            
blockrec    : blockrec statement {$$ = adopt(adopt(adopt2(new_astree(TOK_blockrec, $1->filenr, $1->linenr, $1->offset, "blockrec"), $1, $2), $1), $2);}
            | statement          {$$ = adopt(adopt1(new_astree(TOK_blockrec, $1->filenr,$1->linenr, $1->offset, "blockrec"), $1), $1);}
            ;
            
statement   : block     {$$ = adopt1(new_astree(TOK_STATEMENT, $1->filenr,$1->linenr, $1->offset, "statment"), $1);}
            | vardecl   {$$ = adopt1(new_astree(TOK_STATEMENT, $1->filenr,$1->linenr, $1->offset, "statment"), $1);}
            | while     {$$ = adopt1(new_astree(TOK_STATEMENT, $1->filenr,$1->linenr, $1->offset, "statment"), $1);}
            | ifelse    {$$ = adopt1(new_astree(TOK_STATEMENT, $1->filenr,$1->linenr, $1->offset, "statment"), $1);}
            | return    {$$ = adopt1(new_astree(TOK_STATEMENT, $1->filenr,$1->linenr, $1->offset, "statment"), $1);}
            | expr ';'  {$$ = adopt(adopt1(new_astree(TOK_STATEMENT, $1->filenr,$1->linenr, $1->offset, "statment"), $1), $1);}
            ;
            
vardecl     : type TOK_IDENT '=' expr ';' {$$ = adopt(adopt1(adopt2(new_astree(TOK_VARDECL, $1->filenr,$1->linenr, $1->offset, "vardecl"), $1, $2), $4),$4);}
            ;
            
while       : TOK_WHILE '(' expr ')' statement {$$ = adopt(adopt(adopt1(adopt1(new_astree(TOK_WHILE, $1->filenr,$1->linenr, $1->offset, "while"), $3), $5), $3), $5);}                   
            ;
            
ifelse      : TOK_IF '(' expr ')' statement TOK_ELSE statement {$$ = adopt(adopt(adopt(adopt1(new_astree(TOK_IFELSE,
                                                                     $1->filenr,$1->linenr, $1->offset, "ifelse"), $3), $3), $5), $7);}
            | TOK_IF '(' expr ')' statement                    {$$ = adopt(adopt(adopt1(new_astree(TOK_IFELSE,
                                                                     $1->filenr,$1->linenr, $1->offset, "ifelse"), $3), $3), $5);}
            ;
            
return      : TOK_RETURN expr ';' {$$ = adopt(adopt1(new_astree(TOK_RET, $1->filenr,$1->linenr, $1->offset, "return"), $2), $2);}
            | TOK_RETURN ';'      {$$ = adopt1(new_astree(TOK_RET, $1->filenr,$1->linenr, $1->offset, "return"),$2);}
            ;

expr        : binop                        {$$ = adopt1(new_astree(TOK_EXPR, 
                                                   $1->filenr,$1->linenr, $1->offset, "expr"), $1);}              
            | unop %prec TOK_POS           {$$ = adopt1(new_astree(TOK_EXPR,
                                                   $1->filenr,$1->linenr, $1->offset, "expr"), $1);}      
            | allocator %prec TOK_NEW      {$$ = adopt1(new_astree(TOK_EXPR,
                                                   $1->filenr,$1->linenr, $1->offset, "expr"), $1);}      
            | call %prec TOK_CALL          {$$ = adopt1(new_astree(TOK_EXPR,
                                                   $1->filenr,$1->linenr, $1->offset, "expr"), $1);}      
            | '(' expr ')' %prec TOK_PAREN {$$ = adopt(adopt1(new_astree(TOK_EXPR,
                                                   $1->filenr,$1->linenr, $1->offset, "expr"), $2), $2);}
            | variable %prec TOK_ARRAY     {$$ = adopt1(new_astree(TOK_EXPR,
                                                   $1->filenr,$1->linenr, $1->offset, "expr"), $1);}      
            | constant                     {$$ = adopt1(new_astree(TOK_EXPR, $1->filenr,$1->linenr, $1->offset, "expr"), $1);}      
            ;

binop       : expr '=' expr     {$$ = adopt(adopt1(adopt1(adopt(adopt1(new_astree(TOK_BINOP,
                                       $1->filenr,$1->linenr, $1->offset, "binop"), $1), $1), $2), $3), $3);}
            | expr TOK_EQ expr  {$$ = adopt(adopt1(adopt1(adopt(adopt1(new_astree(TOK_BINOP,
                                       $1->filenr,$1->linenr, $1->offset, "binop"), $1), $1), $2), $3), $3);}
            | expr TOK_NE expr  {$$ = adopt(adopt1(adopt1(adopt(adopt1(new_astree(TOK_BINOP,
                                       $1->filenr,$1->linenr, $1->offset, "binop"), $1), $1), $2), $3), $3);}
            | expr '<' expr     {$$ = adopt(adopt1(adopt1(adopt(adopt1(new_astree(TOK_BINOP,
                                       $1->filenr,$1->linenr, $1->offset, "binop"), $1), $1), $2), $3), $3);}
            | expr TOK_LE expr  {$$ = adopt(adopt1(adopt1(adopt(adopt1(new_astree(TOK_BINOP,
                                       $1->filenr,$1->linenr, $1->offset, "binop"), $1), $1), $2), $3), $3);}
            | expr '>' expr     {$$ = adopt(adopt1(adopt1(adopt(adopt1(new_astree(TOK_BINOP,
                                       $1->filenr,$1->linenr, $1->offset, "binop"), $1), $1), $2), $3), $3);}
            | expr TOK_GE expr  {$$ = adopt(adopt1(adopt1(adopt(adopt1(new_astree(TOK_BINOP,
                                       $1->filenr,$1->linenr, $1->offset, "binop"), $1), $1), $2), $3), $3);}
            | expr '+' expr     {$$ = adopt(adopt1(adopt1(adopt(adopt1(new_astree(TOK_BINOP,
                                       $1->filenr,$1->linenr, $1->offset, "binop"), $1), $1), $2), $3), $3);}
            | expr '-' expr     {$$ = adopt(adopt1(adopt1(adopt(adopt1(new_astree(TOK_BINOP,
                                       $1->filenr,$1->linenr, $1->offset, "binop"), $1), $1), $2), $3), $3);}
            | expr '*' expr     {$$ = adopt(adopt1(adopt1(adopt(adopt1(new_astree(TOK_BINOP,
                                       $1->filenr,$1->linenr, $1->offset, "binop"), $1), $1), $2), $3), $3);}
            | expr '/' expr     {$$ = adopt(adopt1(adopt1(adopt(adopt1(new_astree(TOK_BINOP,
                                       $1->filenr,$1->linenr, $1->offset, "binop"), $1), $1), $2), $3), $3);}
            | expr '%' expr     {$$ = adopt(adopt1(adopt1(adopt(adopt1(new_astree(TOK_BINOP,
                                       $1->filenr,$1->linenr, $1->offset, "binop"), $1), $1), $2), $3), $3);}
            ; 

unop        : '+' expr %prec TOK_POS
                       {$$ = adopt(adopt1(adopt1(new_astree(TOK_UNOP,
                           $1->filenr,$1->linenr, $1->offset, "unop"), $1), $2), $2);}
            | '-' expr %prec TOK_NEG
                       {$$ = adopt(adopt1(adopt1(new_astree(TOK_UNOP,
                           $1->filenr,$1->linenr, $1->offset, "unop"), $1), $2), $2);}
            | '!' expr
                       {$$ = adopt(adopt1(adopt1(new_astree(TOK_UNOP,
                           $1->filenr,$1->linenr, $1->offset, "unop"), $1), $2), $2);}
            | TOK_ORD expr
                       {$$ = adopt(adopt1(adopt1(new_astree(TOK_UNOP,
                           $1->filenr,$1->linenr, $1->offset, "unop"), $1), $2), $2);}
            | TOK_CHR expr
                       {$$ = adopt(adopt1(adopt1(new_astree(TOK_UNOP,
                           $1->filenr,$1->linenr, $1->offset, "unop"), $1), $2), $2);}
            ;
            
allocator   : TOK_NEW basetype '(' expr ')' {$$ = adopt(adopt2(new_astree(TOK_ALLOCATOR, $1->filenr,$1->linenr, $1->offset, "allocator" ), $2, $4), $4);}      
            | TOK_NEW basetype '(' ')'      {$$ = adopt1(new_astree(TOK_ALLOCATOR, $1->filenr,$1->linenr, $1->offset, "allocator" ), $2);}                       
            | TOK_NEW basetype '[' expr ']' {$$ = adopt(adopt2(new_astree(TOK_ALLOCATOR, $1->filenr,$1->linenr, $1->offset, "allocator" ), $2, $4), $4);}
            ;
            
            
call        : TOK_IDENT '(' rec ')' {$$ = new_astree(TOK_CALL, $1->filenr,$1->linenr, $1->offset, "call" );adopt1($$,$1);adopt($$, $3);}              
            | TOK_IDENT '(' ')'     {$$ = new_astree(TOK_CALL, $1->filenr,$1->linenr, $1->offset, "call" );adopt1($$, $1);}
            ;
            
rec           : expr       {$$ = new_astree(TOK_rec1, $1->filenr,$1->linenr, $1->offset, "callrec");adopt1($$,$1);adopt($$, $1);}                
            | expr ',' rec {$$ = new_astree(TOK_rec1, $1->filenr,$1->linenr, $1->offset, "callrec");adopt2($$,$1,$3);adopt($$, $3); adopt($$, $1);}
            ;

variable    : TOK_IDENT                          {$$ = new_astree(TOK_VAR, $1->filenr,$1->linenr, $1->offset, "variable" );adopt1($$, $1);}
                                
            | expr '[' expr ']' %prec TOK_ARRAY  {$$ = new_astree(TOK_ARRAY, $1->filenr,$1->linenr, $1->offset, "array" );adopt2($$, $1, $3);adopt($$, $1);adopt($$, $3);}
                                 
            | expr '.' TOK_IDENT %prec TOK_FIELD {$$ = new_astree(TOK_FIELD, $1->filenr,$1->linenr, $1->offset, "field");adopt1($$,$1);adopt($$, $1);adopt1($$,$3);}
            ;

constant    : TOK_INTCON    {$$ = new_astree(TOK_CONSTANT, $1->filenr, $1->linenr, $1->offset, "constant" );adopt1($$, $1);}                
            | TOK_CHARCON   {$$ = new_astree(TOK_CONSTANT, $1->filenr, $1->linenr, $1->offset, "constant" );adopt1($$, $1);}                     
            | TOK_STRINGCON {$$ = new_astree(TOK_CONSTANT, $1->filenr, $1->linenr, $1->offset, "constant" );adopt1($$, $1);}                
            | TOK_FALSE     {$$ = new_astree(TOK_CONSTANT, $1->filenr, $1->linenr, $1->offset, "constant" );adopt1($$, $1);}
            | TOK_TRUE      {$$ = new_astree(TOK_CONSTANT, $1->filenr, $1->linenr, $1->offset, "constant" );adopt1($$, $1);}
            | TOK_NULL      {$$ = new_astree(TOK_CONSTANT, $1->filenr, $1->linenr, $1->offset, "constant" );adopt1($$, $1);}
            ;


%%

const char *get_yytname (int symbol) {
    return yytname [YYTRANSLATE (symbol)];
}


bool is_defined_token (int symbol) {
    return YYTRANSLATE (symbol) > YYUNDEFTOK;
}

