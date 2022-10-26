keyv1: lex.yy.c y.tab.c
	gcc -o keyv1 y.tab.c
y.tab.c: keyv1.y
	yacc keyv1.y
lex.yy.c: key.l
	lex key.l
