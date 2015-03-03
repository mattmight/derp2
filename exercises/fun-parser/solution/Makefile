all: fun-parser

fun.yacc.sx: fun.grammar.sx
	derp2 fun.grammar.sx

fun-parser.rkt: fun-prefix.rkt fun-suffix.rkt fun.yacc.sx fun-definitions.rkt
	cat fun-prefix.rkt fun.yacc.sx fun-suffix.rkt > fun-parser.rkt

fun-parser: fun-parser.rkt
	raco exe fun-parser.rkt

.PHONY: clean
clean:
	rm -fv fun.yacc.sx fun-parser fun-parser.rkt


