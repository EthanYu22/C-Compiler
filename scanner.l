%{
// $Id: scanner.l,v 1.3 2013-10-15 16:37:56-07 - - $

#include <stdlib.h>
#include "auxlib.h"
#include "lyutils.h"

#define YY_USER_ACTION { scanner_useraction (); }
#define IGNORE(THING) { }

%}

%option 8bit
%option backup
%option debug
%option ecs
%option nodefault
%option nounput
%option noyywrap
%option perf-report
%option verbose

LETTER [A-Za-z_]
CHAR_CONST ('([^\\'\n]|\\[\\'"0nt])')
BAD_CHAR ('([^\\'\n]|\\[\\'"0nt]|\\[^\\'"0nt])?[']?[\n]?)
STR_CONST (\"([^\\"\n]|\\[\\'"0nt])*\")
BAD_STR (\"([^\\"\n]|\\[^\\'"0nt])*[\"]?[\n]?)
COMMENT   (\/\/.*)
DIGIT [0-9]
TOK_INT ({DIGIT}+)
TOK_IDENT ({LETTER}({LETTER}|{DIGIT})*)
BAD_IDENT ({DIGIT}({LETTER}|{DIGIT})*)

%%

"#".* { scanner_include(); }
[ \t]+ { IGNORE (white space); }
\n { scanner_newline(); }


"void"                           { return yylval_token (TOK_VOID); }
"bool"                           { return yylval_token (TOK_BOOL); }
"char"                           { return yylval_token (TOK_CHAR); }
"int"                            { return yylval_token (TOK_INT); }
"string"                         { return yylval_token (TOK_STRING); }
"struct"                         { return yylval_token (TOK_STRUCT); }
"if"                             { return yylval_token (TOK_IF); }
"else"                           { return yylval_token (TOK_ELSE); }
"while"                          { return yylval_token (TOK_WHILE); }
"return"                         { return yylval_token (TOK_RETURN); }
"false"                          { return yylval_token (TOK_FALSE); }
"true"                           { return yylval_token (TOK_TRUE); }
"null"                           { return yylval_token (TOK_NULL); }
"new"                            { return yylval_token (TOK_NEW); }
"ord"                            { return yylval_token (TOK_ORD); }
"chr"                            { return yylval_token (TOK_CHR); }

"="                         { return yylval_token ('='); }
"+"                         { return yylval_token ('+'); }
"-"                         { return yylval_token ('-'); }
"*"                         { return yylval_token ('*'); }
"/"                         { return yylval_token ('/'); }
"("                         { return yylval_token ('('); }
")"                         { return yylval_token (')'); }
";"                         { return yylval_token (';'); }
"["                         { return yylval_token ('['); }
"]"                         { return yylval_token (']'); }
","                         { return yylval_token (','); }
"."                         { return yylval_token ('.'); }
"!"                         { return yylval_token ('!'); }
"{"                         { return yylval_token ('{'); }
"}"                         { return yylval_token ('}'); }
"%"                         { return yylval_token ('%'); }
"!"                         { return yylval_token ('!'); }
"<"                          { return yylval_token ('<'); }
">"                          { return yylval_token ('>'); }
"=="                         { return yylval_token (TOK_EQ); }
"!="                         { return yylval_token (TOK_NE); }
"<="                         { return yylval_token (TOK_LE); }
">="                         { return yylval_token (TOK_GE); }
"[]"                         { return yylval_token (TOK_NEWARRAY); }

{TOK_INT}                         { return yylval_token (TOK_INTCON); }
{CHAR_CONST}                      { return yylval_token (TOK_CHARCON); }
{STR_CONST}                       { return yylval_token (TOK_STRINGCON); }

{TOK_IDENT}                       { return yylval_token (TOK_IDENT); }

{BAD_CHAR}                        { scanner_badchar(*yytext); }
{BAD_STR}                         { scanner_badchar(*yytext); }
{BAD_IDENT}                       { scanner_badchar(*yytext); }


. { scanner_badchar (*yytext); }

%%


RCSC("$Id: scanner.l,v 1.3 2013-10-15 16:37:56-07 - - $")
