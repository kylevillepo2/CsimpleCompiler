%option yylineno
%pointer

%{
    #include <cstdlib>
    #include <cstring>
    #include "ast.hpp"
    #include "parser.hpp"

    void yyerror(const char *); 
%}

/** WRITE ME:
 *  Put any definitions you need here. Add your own regular expressions
 *  below for ALL tokens defined in the manual. You can choose not to use
 *  definitions here.
 */

DIGIT   [0-9]
LETTER  [A-Za-z]
ID      {LETTER}({LETTER}|{DIGIT}|_)*
WS      [ \t\r\n]+
HEX     (0[xX][0-9A-Fa-f]+)
OCT     (0[0-7]+)
BIN     ([01]+[bB])
DEC     ([1-9][0-9]*|0)
CHAR    \'[[:print:]]\'
STRING  \"([^\"\n]*)\"

%x comment

/** The example below is very simple, but it shows almost everything you will
 *  need to do for this first part. The syntax is as follows:
 *
 *  regular_expression      {  c code  }
 *
 * The lexer will try to match each regular expression. When it does it will
 * execute the corresponding C code.
*/

%%

 /* WRITEME: Put your code from Project 3 here */
"/%"                   { BEGIN(comment); }
<comment>"%/"          { BEGIN(INITIAL); }
<comment>[^%]+         { /* skip */ }
<comment>"%"           { /* skip */ }
<comment><<EOF>>       { yyerror("Unfinished comment"); }

{WS}                   ;

"boolean"              { return BOOLEAN; }
"char"                 { return CHAR; }
"integer"              { return INTEGER; }
"string"               { return STRING; }
"intptr"               { return INTPTR; }
"charptr"              { return CHARPTR; }
"if"                   { return IF; }
"else"                 { return ELSE; }
"while"                { return WHILE; }
"var"                  { return VAR; }
"procedure"            { return PROCEDURE; }
"return"               { return RETURN; }
"null"                 { return NULLTOKEN; }
"true"                { yylval.u_base_int = 1; return TRUE;  }
"false"               { yylval.u_base_int = 0; return FALSE; }

"&&"                   { return AND; }
"||"                   { return OR; }
"=="                   { return EQ; }
"!="                   { return NEQ; }
"<="                   { return LE; }
">="                   { return GE; }
"<"                    { return LT; }
">"                    { return GT; }
"="                    { return ASSIGN; }
"+"                    { return PLUS; }
"-"                    { return MINUS; }
"*"                    { return MULT; }
"/"                    { return DIV; }
"!"                    { return NOT; }
"&"                    { return AMPERSAND; }
"^"                    { return CARET; }
"|"                    { return BAR; }

";"                    { return SEMICOLON; }
":"                    { return COLON; }
","                    { return COMMA; }
"{"                    { return LBRACE; }
"}"                    { return RBRACE; }
"("                    { return LPAREN; }
")"                    { return RPAREN; }
"["                    { return LBRACK; }
"]"                    { return RBRACK; }

{HEX}                  { yylval.u_base_int = strtol(yytext, NULL, 16); return INT_LITERAL; }
{OCT}                  { yylval.u_base_int = strtol(yytext, NULL, 8); return INT_LITERAL; }
{BIN}                  { yylval.u_base_int = strtol(yytext, NULL, 2); return INT_LITERAL; }
{DEC}                  { yylval.u_base_int = atoi(yytext); return INT_LITERAL; }
{CHAR}                 { yylval.u_base_int = yytext[1]; return CHAR_LITERAL; }
{STRING} {
    int len = strlen(yytext);
    // Allocate space for the string without quotes and with null terminator
    char* str = (char*)malloc(len - 1); // -2 for quotes, +1 for '\0'
    if (len >= 2) {
        strncpy(str, yytext + 1, len - 2); // Copy without the first and last quote
        str[len - 2] = '\0';
    } else {
        str[0] = '\0'; // Handle empty string ""
    }
    yylval.u_base_charptr = str;
    return STRING_LITERAL;
}

{ID}                   { yylval.u_base_charptr = strdup(yytext); return IDENTIFIER; }

.                      { yyerror("invalid character"); }

%%

/** You shall not pass!
 *  You should not  have to do or edit anything past this.
 */

int yywrap(void) {
    return 1;
}
