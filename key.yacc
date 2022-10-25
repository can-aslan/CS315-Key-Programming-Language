/* key.y */
%{
#include <stdio.h>
%}
%union {
    #include <stdbool.h>
    double real;
    int integer;
    char string[5000];
    bool boolean;
    char character;    
}
%token <boolean> TRUE_LITERAL
%token <boolean> FALSE_LITERAL
%token MAIN IF FOR WHILE DO ELSE FUNCTION_DEF RETURN NEGATION_OP AND_OP OR_OP
%token GREATER_OP LESS_OP GREAT_OR_EQUAL_OP LESS_OR_EQUAL_OP EQUALITY_OP NOT_EQUALITY_OP
%token BOOLEAN_TYPE INT_TYPE STRING_TYPE CHAR_TYPE CONNECTION_TYPE TIMESTAMP_TYPE DOUBLE_TYPE
%token TIMESTAMP_PF TEMPERATURE_PF HUMIDITY_PF AIR_PRESSURE_PF AIR_QUALITY_PF LIGHT_PF SOUND_LEVEL_PF SEND_PF READ_PF
%token DOUBLE_QUOTE LP RP NL LBRACE RBRACE TAB BACKSLASH COMMA SINGLE_LINE_COMMENT MULT_LINE_COMMENT SEMICOLON
%token PLUS_OP MINUS_OP DIV_OP MULT_OP POWER_OP INCREMENT_OP DECREMENT_OP ASSIGN_OP  MODULO_OP 
%token SWITCH_IDENTIFIER
%token <integer> INTEGER
%token <real> DOUBLE
%token <string> STRING
%token <string> IDENTIFIER
%%
test: STRING '\n' {printf("%s, string is typed", $1); {return 0;}}

%%
#include "lex.yy.c"
int lineno = 0;
int yyerror(char *s) {
    printf("%s is poo poo", s);
}
int main(void) {
    return yyparse();
}