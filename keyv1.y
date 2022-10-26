/* keyv1.y */
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
%token LP RP NL LBRACE RBRACE COMMA SINGLE_LINE_COMMENT MULT_LINE_COMMENT SEMICOLON
%token PLUS_OP MINUS_OP DIV_OP MULT_OP POWER_OP INCREMENT_OP DECREMENT_OP ASSIGN_OP  MODULO_OP 
%token SWITCH_IDENTIFIER DOT ESTABLISH_CONN_PF VOID DOUBLE_QUOTE TAB BACKSLASH CONCAT_OP
%token <integer> INTEGER
%token <real> DOUBLE
%token <string> STRING
%token <character> CHAR
%token <string> IDENTIFIER
%%
program: MAIN LP RP option_nl LBRACE stmt_list_with_if RBRACE option_nl
         | error NL {
                        printf(" in line %d!\n", lineno);
                        yyerrok;
                    }
         ;

option_nl: /*empty*/
           | NL option_nl;

stmt_list_with_if: stmt_with_if
                   | stmt_with_if stmt_list_with_if
                   ;

stmt_with_if: stmt;
stmt: decrement_expr
      | increment_expr
      | SINGLE_LINE_COMMENT
      | declaration_expr
      | return_expr
      | NL
      | error NL {
                     printf(" in line %d!\n", lineno);
                     yyerrok;
                 }
      ;
      
decrement_expr: decrement_operation SEMICOLON;
decrement_operation: IDENTIFIER DECREMENT_OP;
                     | DECREMENT_OP IDENTIFIER
                     ;
                     
increment_expr: increment_operation SEMICOLON;
increment_operation: IDENTIFIER INCREMENT_OP
			         | INCREMENT_OP IDENTIFIER
                     ;

declaration_expr: INT_TYPE IDENTIFIER SEMICOLON; 

return_expr: RETURN return_stmt SEMICOLON;

return_stmt: /* empty */
             | str_stmt_return
             | num_comparable
             ;

str_stmt: STRING  
          | IDENTIFIER
          | STRING CONCAT_OP str_stmt
          | IDENTIFIER CONCAT_OP str_stmt
          ;

str_stmt_return: STRING
                 | STRING CONCAT_OP str_stmt
                 | IDENTIFIER CONCAT_OP str_stmt
                 ;

num_comparable: CHAR  
                | math_stmt
                ;

math_stmt: math_stmt PLUS_OP term
           | math_stmt MINUS_OP term
           | term
           ;

/* term: factor
      | term MULT_OP factor
      | term DIV_OP factor
      | term MODULO_OP factor
      | factor POWER_OP term
      ; */

term: power
      | term MULT_OP power
      | term DIV_OP power
      | term MODULO_OP power       
      ;
      //(3 ** 5) / 3

power: power POWER_OP factor
       | factor
       ;
 
factor: INTEGER
        | DOUBLE
        | IDENTIFIER
        | fcn_call
        | primitive_functions
        | LP math_stmt RP
        ;

param_list_no_type: /* empty */   
                    | IDENTIFIER
                    | IDENTIFIER COMMA param_list_no_type
                    ;


fcn_name: IDENTIFIER;

/*
fcn_return_type: INT_TYPE 
                 | VOID
                 ; */

fcn_call: fcn_name LP param_list_no_type RP;

primitive_functions: get_timestamp
				     | get_temperature
				     | get_humidity
				     | get_air_pressure
				     | get_air_quality
				     | get_light
				     | get_sound_level
                     | send
                     | read
                     | establish_connection
                     ;
				
get_timestamp: TIMESTAMP_PF LP RP;
get_temperature: TEMPERATURE_PF LP RP;
get_humidity: HUMIDITY_PF LP RP;
get_air_pressure: AIR_PRESSURE_PF LP RP;
get_air_quality: AIR_QUALITY_PF LP RP;
get_light: LIGHT_PF LP RP;
get_sound_level: SOUND_LEVEL_PF LP frequency RP;
send: SEND_PF LP send_item RP; // UPDATE WITH CONNECTION VAR
read: READ_PF LP RP; // UPDATE WITH CONNECTION VAR
establish_connection: ESTABLISH_CONN_PF LP str_stmt RP;

send_item: INTEGER | IDENTIFIER;
frequency: IDENTIFIER
         | DOUBLE
         ;

%%
#include "lex.yy.c"
int lineno = 0;
int yyerror(char *s) {
    printf("%s is poo poo", s);
}
int main(void) {
    return yyparse();
}
