parser: lex.yy.c y.tab.c
	gcc -o parser y.tab.c
y.tab.c: CS315f22_team42.yacc
	yacc CS315f22_team42.yacc
lex.yy.c: CS315f22_team42.lex
	lex CS315f22_team42.lex
