#lang racket

(require derp2)

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


(define (process-binops base ops)
  (match ops 
    ['()
     base]

    [(cons (list op exp) rest)
     (process-binops `(,(string->symbol op) ,base ,exp) rest)]))

(define (process-apps base trailers)
  (match trailers
    ['()  base]
    
    [(cons head tail)
     (process-apps `(,base ,head) tail)]))

(define fun-parser
  (derp2-parser 

    (tokens LIT PUNCT KEYWORD ID END)

    (start program)

    (end EOF)

    (grammar
      [program  (rep+ stmt)]

      ; Add/finish reductions to fit stmt into 
      ; the following grammar:

      ; <stmt> ::= (print <expr>)
      ;         |  (define <symbol> <expr>)
      ;         |  (define (<symbol> <symbol>*) <expr>)

      [stmt     (or ($--> (seq "print" expr ";")
                          `(print ,($ 2)))

                    ($--> (seq 'ID "=" expr ";")
                          ; ($ 1) will be bound to the value of 'ID
                          ; ($ 3) will be bound to the value of app.
                          `(define ,(string->symbol ($ 1)) ,($ 3)))

                    ($--> (seq 'ID "(" params ")" "=" expr ";")
                          ; ($ 1) will be name of the function
                          ; ($ 3) will be the parameters to it
                          ; ($ 6) will be the body of the def
                          `(define (,(string->symbol ($ 1)) ,@($ 3)) ,($ 6))))]


      [params   ($--> (rep/sep "," 'ID #f)
                      (map string->symbol $$))]

      ; Add/finish reductions below so that expr fits the
      ; following grammar:

      ; <expr> ::= (<expr> <expr>)        
      ;         |  (lambda (<symbol>*) <expr>)
      ;         |  (if <expr> <expr> <expr>)
      ;         |  (+ <expr> <expr>)
      ;         |  (- <expr> <expr>)
      ;         |  (* <expr> <expr>)
      ;         |  (/ <expr> <expr>)
      ;         |  <number>
      ;         |  <sybol>

      ; Function call (is juxtaposition): 
      ; ex: f x y = (f(x))(y)
      [expr     ($--> (seq lamtest (rep lamtest))
                      (process-apps ($ 1) ($ 2)))]

      ; Lambda forms:
      [lamtest  (or ($--> (seq "lambda" params ":" lamtest)
                          `(lambda ,($ 2) ,($ 4)))
                    test)]

      ; If conditions:
      [test     (or ($--> (seq "if" expr "then" expr "else" expr "fi")
                          `(if ,($ 2) ,($ 4) ,($ 6)))
                    sum)]

      ; Sums (or differences):
      [sum      ($--> (seq term (rep (seq (or "+" "-") term)))
                      (process-binops ($ 1) ($ 2)))]

      ; Terms:
      [term     ($--> (seq factor (rep (seq (or "*" "/") factor)))
                      ; ($ 1) will be bound to result of factor
                      ; ($ 2) will be a list looking like:
                      ;   (("*"|"-" <factor>) ...)
                      ;  e.g.:
                      ;   (("*" 10) ("/" 4) ("*" 100))
                      (process-binops ($ 1) ($ 2)))]

      ; Factors:
      [factor   (or (car #'( "(" #,expr ")" ))
                    ; In the above: 

                    ;  #'( "(" #,expr ")" )

                    ; is equivalent to:

                    ;  ($--> (seq "(" expr ")")
                    ;        (list ($ 2)))

                    ; and the (car ...) grabs the first element
                    ; of that list, which is ($ 2).

                    'NUM
                    ($--> 'ID (string->symbol $$)))]
      )))


(write (fun-parser (fun-token-generator (current-input-port))))
(newline)

