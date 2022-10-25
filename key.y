/* key.y */
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
%token SWITCH_IDENTIFIER DOT ESTABLISH_CONN_PF VOID 
%token <integer> INTEGER
%token <real> DOUBLE
%token <string> STRING
%token <string> IDENTIFIER
%%
program: MAIN LP RP LBRACE stmt_list_with_if RBRACE NL    {printf("Input is valid!"); return 0;}
stmt_list_with_if: stmt_with_if
                 | stmt_with_if stmt_list_with_if
                 ;

stmt: 	assignment_expr
| declaration_expr
| loop_expr
| increment_expr
| decrement_expr
| SINGLE_LINE_COMMENT
| MULT_LINE_COMMENT
| return_stmt
;

stmt_list: stmt | stmt stmt_list;
stmt_with_if: stmt | if_stmt; 

if_stmt: matched | unmatched;

matched:    IF LP boolean_list RP LBRACE matched RBRACE ELSE LBRACE matched RBRACE
		    | stmt_list matched
		    | stmt_list
            ;

unmatched: IF LP boolean_list RP LBRACE stmt_with_if RBRACE
            | IF LP boolean_list RP LBRACE matched RBRACE ELSE LBRACE unmatched RBRACE
            | IF LP boolean_list RP LBRACE unmatched RBRACE ELSE LBRACE unmatched RBRACE
            | IF LP boolean_list RP LBRACE unmatched RBRACE ELSE LBRACE matched RBRACE
            | stmt_list unmatched
            | stmt_list
            ;

boolean_list: boolean_list OR_OP and_term
		    | and_term
            ;

and_term: and_term AND_OP boolean_factor
           | boolean_factor
           ;

boolean_factor: boolean_expr
             	| LP boolean_list RP
                ;

boolean_expr: NEGATION_OP boolean_expr
                   | num_compr_expr
                   | string_compr_expr
                   | boolean_literal
			       | boolean_var
                   ;
                                      
num_compr_expr: num_comparable comparator_operator num_comparable SEMICOLON;

num_comparable: int_var
               | char_var
			   | INTEGER
			   | DOUBLE
			   | math_stmt
			   | double_var
               ;
               
string_compr_expr: string_var equal_or_not_operator string_var SEMICOLON;

comparator_operator: GREATER_OP
                    | LESS_OP
                    | GREAT_OR_EQUAL_OP
                    | LESS_OR_EQUAL_OP
                    | equal_or_not_operator
                    ;

arithmetic_operator: PLUS_OP
                    | MINUS_OP
                    | MULT_OP
                    | DIV_OP
                    | MODULO_OP
                    | POWER_OP
                    ;

equal_or_not_operator : EQUALITY_OP
                      | NOT_EQUALITY_OP
                      ;
                      
declaration_expr: var_type IDENTIFIER SEMICOLON
			      | var_type assignment_expr
                  ;

var_type:   INT_TYPE
            | CHAR_TYPE
            | STRING_TYPE
            | CONNECTION_TYPE
            | BOOLEAN_TYPE
            | TIMESTAMP_TYPE
            | DOUBLE_TYPE
            ;

int_var: IDENTIFIER | fcn_call;
char_var: IDENTIFIER | fcn_call;
string_var: IDENTIFIER | fcn_call;
connection_var: IDENTIFIER | fcn_call;
timestamp_var: IDENTIFIER | fcn_call;
boolean_var: IDENTIFIER | fcn_call;
double_var: IDENTIFIER | fcn_call;
boolean_literal: TRUE_LITERAL | FALSE_LITERAL;

str_stmt:   STRING
            | STRING PLUS_OP str_stmt
            | IDENTIFIER PLUS_OP str_stmt
            | IDENTIFIER
            ;

assignment_expr: assignment_expr_no_sc SEMICOLON;        
assignment_expr_no_sc: IDENTIFIER ASSIGN_OP assignment_operand;

assignment_operand: math_stmt
                         | str_stmt
				         | IDENTIFIER
                         | boolean_literal
                         | boolean_list
				         | primitive_functions
                         | establish_connection
                         | SWITCH_IDENTIFIER
                         | fcn_call
                         ;

loop_expr: while_loop
		    | for_loop
		    | do_while_loop
            ;

increment_expr: increment_operation SEMICOLON;

increment_operation: IDENTIFIER INCREMENT_OP
			        | INCREMENT_OP IDENTIFIER
                    ;

decrement_expr: decrement_operation SEMICOLON;

decrement_operation: IDENTIFIER DECREMENT_OP
			        | DECREMENT_OP IDENTIFIER
                    ;

while_loop: WHILE LP boolean_list RP LBRACE stmt_list_with_if RBRACE;

do_while_loop: DO LBRACE stmt_list_with_if RBRACE WHILE LP boolean_list RP SEMICOLON;

math_stmt: math_stmt PLUS_OP term
            | math_stmt MINUS_OP term
		    | term
            ;

term: term MULT_OP factor
	    | term DIV_OP factor
        | term MODULO_OP factor
        | factor POWER_OP term
        | factor
        ;

factor: INTEGER
             | DOUBLE
             | IDENTIFIER
             | fcn_call
		     | primitive_functions
             | LP math_stmt RP
             ;

for_loop: FOR LP for_expr RP LBRACE stmt_list_with_if LBRACE;
for_expr: for_init SEMICOLON boolean_list SEMICOLON for_update;
for_init: var_type assignment_expr_no_sc
		 | assignment_expr_no_sc
         ;

for_update: math_stmt
                 | assignment_expr_no_sc
		         | decrement_operation
                 | increment_operation
                 ;

define_fcn: FUNCTION_DEF fcn_return_type fcn_name LP param_list RP RBRACE stmt_list_with_if LBRACE;

param_list: empty    
            | var_type IDENTIFIER
            | var_type IDENTIFIER COMMA param_list
            ;

param_list_no_type: empty   
                | IDENTIFIER
                | IDENTIFIER COMMA param_list_no_type
                ;


fcn_name: IDENTIFIER;
fcn_return_type: var_type 
    | VOID
    ;
 
fcn_call: fcn_name LP param_list_no_type RP;
fcn_call_expr: fcn_call  SEMICOLON;

primitive_function_expr: primitive_functions SEMICOLON;
primitive_functions: get_timestamp
				| get_temperature
				| get_humidity
				| get_air_pressure
				| get_air_quality
				| get_light
				| get_sound_level
                ;
				
get_timestamp: TIMESTAMP_PF LP RP;
get_temperature: TEMPERATURE_PF LP RP;
get_humidity: HUMIDITY_PF LP RP;
get_air_pressure: AIR_PRESSURE_PF LP RP;
get_air_quality: AIR_QUALITY_PF LP RP;
get_light: LIGHT_PF LP RP;
get_sound_level: SOUND_LEVEL_PF LP frequency RP;

frequency: IDENTIFIER
         | DOUBLE
         ;

establish_connection_expr: establish_connection SEMICOLON;
establish_connection: ESTABLISH_CONN_PF LP str_stmt RP;

send_data_expr: send_data SEMICOLON;
send_data: connection_var DOT SEND_PF LP send_item RP
                | establish_connection DOT SEND_PF LP send_item RP
                ;

receive_data_expr: receive_data SEMICOLON;
receive_data :  connection_var DOT READ_PF LP RP
                | establish_connection DOT READ_PF LP RP
                ;

send_item: int_var
           | INTEGER
           ;

switch_stmt: SWITCH_IDENTIFIER ASSIGN_OP boolean_literal SEMICOLON
                  | SWITCH_IDENTIFIER ASSIGN_OP SWITCH_IDENTIFIER SEMICOLON
			      | SWITCH_IDENTIFIER ASSIGN_OP NEGATION_OP SWITCH_IDENTIFIER SEMICOLON 
                  ;

return_expr: return_stmt SEMICOLON
return_stmt: RETURN
            | RETURN factor
			| RETURN num_comparable
            | RETURN str_stmt
            ;

empty: /* EMPTY */;

%%
#include "lex.yy.c"
int lineno = 0;
int yyerror(char *s) {
    printf("%s is poo poo\n", s);
}
int main(void) {
    return yyparse();
}