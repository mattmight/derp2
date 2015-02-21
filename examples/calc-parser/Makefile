all: calc-parser

calc.yacc.sx: calc.grammar.sx
	derp2 calc.grammar.sx

calc-parser.rkt: calc-prefix.rkt calc-suffix.rkt calc.yacc.sx
	cat calc-prefix.rkt calc.yacc.sx calc-suffix.rkt > calc-parser.rkt

calc-parser: calc-parser.rkt
	raco exe calc-parser.rkt

.PHONY: clean
clean:
	rm -fv calc.yacc.sx calc-parser calc-parser.rkt


