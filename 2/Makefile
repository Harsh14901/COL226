file := "./test"
build:
	mlyacc bool_parser.yacc 
	mllex bool_expr.lex
	mlton a2.mlb
run: a2
	./a2 ${file}
clean:
	-rm bool_parser.yacc.sig
	-rm bool_parser.yacc.sml
	-rm bool_expr.lex.sml

all: build
	-make run file=${file}
	make clean
zip:
	zip 2019CS10431.zip -x .git/* -x .gitignore -x test -x assignment2.pdf -r ./
