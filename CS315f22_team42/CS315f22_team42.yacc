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
%token SWITCH_IDENTIFIER DOT ESTABLISH_CONN_PF VOID DOUBLE_QUOTE TAB BACKSLASH CONCAT_OP STR_EQUALITY_OP STR_NOT_EQUALITY_OP
%token <integer> INTEGER
%token <real> DOUBLE
%token <string> STRING
%token <character> CHAR
%token <string> IDENTIFIER
%%
// Since YACC tool selects the first rule as the start (root) if not specified,
// the rule below ("program") is the beginning of the execution
program: option_nl MAIN LP RP option_nl LBRACE stmt_list RBRACE option_nl 
// { printf("Input is valid!\n"); }
         | error NL {
              printf("syntax error on line %d!\n", lineno);
              isValid = 1;
              yyerrok;
           }
         ;

option_nl: /*empty*/
           | option_nl NL
           ;

stmt_list: stmt
           | stmt_list stmt
           ;

conditional: if_stmt
             | if_else_stmt
             ;

if_stmt: IF LP boolean_list RP option_nl LBRACE stmt_list RBRACE SEMICOLON;

if_else_stmt: IF LP boolean_list RP option_nl LBRACE stmt_list RBRACE option_nl ELSE option_nl LBRACE stmt_list RBRACE SEMICOLON;

stmt: decrement_expr
      | increment_expr
      | SINGLE_LINE_COMMENT
      | MULT_LINE_COMMENT
      | declaration_expr
      | return_expr
      | assignment_expr
      | loop_expr
      | define_fcn
      | fcn_call_expr
      | primitive_function_expr
      | conditional
      | NL
      | error NL {
              printf("syntax error on line %d!\n", lineno);
              isValid = 1;
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

define_fcn: FUNCTION_DEF fcn_return_type fcn_name LP param_list RP option_nl LBRACE stmt_list RBRACE;

param_list: /* empty */
            | var_type IDENTIFIER
            | var_type IDENTIFIER COMMA param_list
            ;

fcn_return_type: var_type 
                 | VOID
                 ; 

loop_expr: while_loop
           | do_while_loop
           | for_loop
           ;

while_loop: WHILE option_nl LP boolean_list RP option_nl LBRACE stmt_list RBRACE;

do_while_loop: DO option_nl LBRACE stmt_list RBRACE option_nl WHILE option_nl LP boolean_list RP SEMICOLON;

for_loop: FOR option_nl LP for_expr RP option_nl LBRACE stmt_list RBRACE;
for_expr: for_init SEMICOLON boolean_list SEMICOLON for_update;
for_init: var_type assignment_expr_no_sc
		  | assignment_expr_no_sc
          ;

for_update: math_stmt
            | assignment_expr_no_sc
		    | decrement_operation
            | increment_operation
            ;

declaration_expr: var_type IDENTIFIER SEMICOLON;
                  | var_type assignment_expr;

assignment_expr: assignment_expr_no_sc SEMICOLON;

assignment_expr_no_sc: IDENTIFIER ASSIGN_OP assignment_operand;

assignment_operand: str_stmt_return
                    | SWITCH_IDENTIFIER
                    | boolean_list
                    ;

boolean_list: boolean_list OR_OP and_term
		      | and_term
              ;

and_term: and_term AND_OP boolean_factor
          | boolean_factor
          ;

boolean_factor: boolean_expr
                | boolean_no_math
                ;

boolean_no_math: NEGATION_OP boolean_no_math
             	 | LP boolean_list OR_OP and_term RP 
             	 | LP and_term AND_OP boolean_factor RP 
                 | LP boolean_literal RP
                 | LP NEGATION_OP boolean_expr RP
                 | LP num_compr_expr RP
                 | LP string_compr_expr RP
                 | LP boolean_no_math RP
                 ;

boolean_expr: boolean_literal
              | NEGATION_OP boolean_expr
              | num_compr_expr
              | num_comparable
              | string_compr_expr
              ;

num_compr_expr: num_comparable comparator_operator num_comparable;

string_compr_expr: str_stmt equal_or_not_operator str_stmt;

comparator_operator: GREATER_OP
                     | LESS_OP
                     | GREAT_OR_EQUAL_OP
                     | LESS_OR_EQUAL_OP
                     | EQUALITY_OP
                     | NOT_EQUALITY_OP
                     ;

equal_or_not_operator: STR_EQUALITY_OP
                       | STR_NOT_EQUALITY_OP
                       ;

var_type: INT_TYPE
          | CHAR_TYPE
          | STRING_TYPE
          | CONNECTION_TYPE
          | BOOLEAN_TYPE
          | TIMESTAMP_TYPE
          | DOUBLE_TYPE
          ;

boolean_literal: TRUE_LITERAL | FALSE_LITERAL;

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
      
fcn_call_expr: fcn_call SEMICOLON;

primitive_function_expr: primitive_functions SEMICOLON;

term: power
      | term MULT_OP power
      | term DIV_OP power
      | term MODULO_OP power       
      ;

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
                    | param_list_no_type_item
                    | param_list_no_type_item COMMA param_list_no_type
                    ;

param_list_no_type_item: num_comparable
                         | str_stmt_return
                         ;

fcn_name: IDENTIFIER;

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
send: IDENTIFIER DOT SEND_PF LP send_item RP
      | establish_connection DOT SEND_PF LP send_item RP
      ;

read: IDENTIFIER DOT READ_PF LP RP;
      | establish_connection DOT READ_PF LP RP;
      ;

establish_connection: ESTABLISH_CONN_PF LP str_stmt RP;

send_item: INTEGER | IDENTIFIER;
frequency: IDENTIFIER
           | DOUBLE
           ;

%%
#include "lex.yy.c"
int lineno = 0;
int isValid = 0;
int yyerror(char *s) {}
int main(void) {
    int result = yyparse();

    if (isValid == 0) {
        printf("Input is valid!\n");
    }

    return result;
}