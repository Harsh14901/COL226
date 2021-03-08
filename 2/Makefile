build:
	mlyacc bool_parser.yacc 
	mllex bool_expr.lex
	mlton a2.mlb
run:
	./a2 test
clean:
	-rm bool_parser.yacc.sig
	-rm bool_parser.yacc.sml
	-rm bool_expr.lex.sml

all: build run clean
