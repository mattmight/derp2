#lang racket


(require parser-tools/lex)
(require (prefix-in : parser-tools/lex-sre))
(require parser-tools/cfg-parser)


;; Lexical analysis
(define-tokens       LIT      (NUM))
(define-tokens       ID       (ID))
(define-empty-tokens KEYWORD  (print def lambda if then else fi))
(define-empty-tokens PUNCT    (= + * - / : |,| |(| |)| |;|
                              ))
(define-empty-tokens END      (EOF))


(define fun-lexer
  (lexer
    [whitespace     (fun-lexer input-port)]

    [(:+ numeric)   (token-NUM (string->number lexeme))]

    ["print"        (token-print)]
    ["lambda"       (token-lambda)]

    ["if"           (token-if)]
    ["then"         (token-then)]
    ["else"         (token-else)]
    ["fi"           (token-fi)]
    
    [(:+ alphabetic)  (token-ID lexeme)]

    ["="            (token-=)]

    [";"            (|token-;|)]
    [":"            (token-:)]
    [","            (|token-,|)]

    ["("            (|token-(|)]
    [")"            (|token-)|)]
    
    ["*"            (token-*)]
    ["+"            (token-+)]
    ["-"            (token--)]
    ["/"            (token-/)]
    
    [(eof)          (token-EOF)]

    ))


(define ((fun-token-generator port))
  (fun-lexer port))



;; Parsing
(include "fun-definitions.rkt")


(define fun-parser
  (cfg-parser 

    (tokens LIT PUNCT KEYWORD ID END)
    
    (start program)

    (end EOF)
    
    (grammar
     
