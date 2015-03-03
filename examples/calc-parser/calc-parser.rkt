#lang racket

(require derp2)

; An example calculator-langugae parser using derp2

; Given an input like:

;   10+(1*5 / 6)

; it will produce an abstract syntax tree like:

;   (+ 10 (/ (* 1 5) 6))


(require parser-tools/lex)
(require (prefix-in : parser-tools/lex-sre))
(require parser-tools/cfg-parser)


;; Lexical analysis
(define-tokens       LIT    (NUM))
(define-empty-tokens PUNCT  (+ * - / |(| |)|))
(define-empty-tokens END    (EOF))


(define calc-lexer
  (lexer
    [whitespace     (calc-lexer input-port)]

    [(:+ numeric)   (token-NUM (string->number lexeme))]

    ["("            (|token-(|)]
    [")"            (|token-)|)]

    ["*"            (token-*)]
    ["+"            (token-+)]
    ["-"            (token--)]
    ["/"            (token-/)]

    [(eof)          (token-EOF)]

    ))


(define ((calc-token-generator port))
  (calc-lexer port))


;; Parsing
(define (process-binops base ops)
  (match ops
    ['()
     base]

    [(cons (list op exp) rest)
     (process-binops `(,(string->symbol op) ,base ,exp) rest)]))


(define calc-parser
  (derp2-parser

    (tokens LIT PUNCT END)

    (start exp)

    (end EOF)

    (grammar
      [exp      ($--> (seq term (rep (seq (or "+" "-") term)))
                      (process-binops ($ 1) ($ 2)))]

      [term     ($--> (seq factor (rep (seq (or "*" "/") factor)))
                      (process-binops ($ 1) ($ 2)))]

      [factor   (or ($--> (seq "(" exp ")")
                          ($ 2))
                    'NUM)]
      )))

(write (calc-parser (calc-token-generator (current-input-port))))
(newline)
