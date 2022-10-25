/* key.l */
digit [0-9]
letter [a-zA-Z]
alphanumeric_or_symbol ({digit}|{letter}|_|$)
all_chars [ |!|#-~|\n|\t]
str_stmt \"{all_chars}*\"
integer {digit}+
double_num {digit}*(\.){digit}+
%%
main    return (MAIN);
if    return (IF);
for    return (FOR);
while    return (WHILE);
do    return (DO);
else    return (ELSE);
function    return (FUNCTION_DEF);
return    return (RETURN);
\!    return (NEGATION_OP);
\&\&    return (AND_OP);
\|\|    return (OR_OP);
\>    return (GREATER_OP);
\<    return (LESS_OP);
\>\=    return (GREAT_OR_EQUAL_OP);
\<\=    return (LESS_OR_EQUAL_OP);
\=\=    return (EQUALITY_OP);
\!\=    return (NOT_EQUALITY_OP);
boolean    return (BOOLEAN_TYPE);
int    return (INT_TYPE);
string    return (STRING_TYPE);
char    return (CHAR_TYPE);
connection    return (CONNECTION_TYPE);
timestamp    return (TIMESTAMP_TYPE);
double    return (DOUBLE_TYPE);
true    return (TRUE_LITERAL);
false    return (FALSE_LITERAL);
getTimestamp    return (TIMESTAMP_PF);
getTemperature    return (TEMPERATURE_PF);
getHumidity    return (HUMIDITY_PF);
getAirPressure    return (AIR_PRESSURE_PF);
getAirQuality    return (AIR_QUALITY_PF);
getLight    return (LIGHT_PF);
getSoundLevel    return (SOUND_LEVEL_PF);
send    return (SEND_PF);
read    return (READ_PF);
establishConnection    return (ESTABLISH_CONN_PF);
void    return (VOID);
\"    return (DOUBLE_QUOTE);
\(    return (LP);
\)    return (RP);
\n    {
        extern int lineno; 
        lineno++;
        return (NL);
      }
\+    return (PLUS_OP);
\-    return (MINUS_OP);
\/    return (DIV_OP);
\*    return (MULT_OP);
\*\*    return (POWER_OP);
\+\+    return (INCREMENT_OP);
\-\-    return (DECREMENT_OP);
\=    return (ASSIGN_OP);
\{    return (LBRACE);
\}    return (RBRACE);
\t    return (TAB);
\%    return (MODULO_OP);
\\    return (BACKSLASH);
\,    return (COMMA);
\.    return (DOT);
\/\/([^\n])*    return (SINGLE_LINE_COMMENT);
\/\*([^(\/\*)]|\*+[^(\*\/)])*([\*])+\/    return (MULT_LINE_COMMENT);
\;    return (SEMICOLON);
{integer}    {   sscanf(yytext, "%d", &yylval);
                 return (INTEGER);
             }

{str_stmt}    {   sscanf(yytext, "%s", &yylval);
                  return (STRING);
              }

{double_num}    {   sscanf(yytext, "%f", &yylval);
                    return (DOUBLE);
                }

switch\[{digit}\]    {   sscanf(yytext[7], "%c", &yylval);
                         return (SWITCH_IDENTIFIER);
                     }

{letter}{alphanumeric_or_symbol}*    {   sscanf(yytext, "%s", &yylval);
                                         return (IDENTIFIER);
                                     }
%%
int yywrap() { return 1; }