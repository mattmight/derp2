#lang racket

; derp2

; derp2 transforms a derp-style grammar into
; a yacc-style grammar, suitable for use 
; with parser-tools/cfg-parser in Racket.

; (It may work with parser-tools/yacc, but there is no 
; guarantee it won't produce shift/reduce conflicts.)


; It supports the common "regular" operations for patterns:

; (seq <pat> ...)
; (or <pat> ...)
; (opt <pat> [ <default> ])
; (rep <pat>)
; (rep+ <pat>)


; It also supports special reduction forms for patterns:

; ($--> <pat> <body>) 
;    where in the body:
;        1. $$ is result of pattern match
;        2. ($ n) is nth element in $$

; ($*--> <pat> ... <exp>)
;    acts like ($--> (seq <pat> ...) <exp>)

; (>--> <pat> <match-clauses>)
;    acts like ($--> <pat> (match $$ <match-clauses>))

; (@--> <pat> <exp>)
;    acts like ($--> <pat> <exp> (apply <exp> $$))

; (cons <pat> <pat>)
;    acts like seq, but with cons instead of list

; (car <pat>)
;    takes the car of the result

; (cdr <pat>)
;    takes the cdr of the result


; It supports repetition-with-separator forms:

; (rep/sep <pat> <pat> <bool>)
; (rep/sep+ <pat> <pat> <bool>)

; rep/sep matches <exp> <sep> ... <sep> <exp> [ <sep> ]
; where zero or more matches of <exp> are allowed.

; rep+/sep matches <exp> <sep> ... <sep> <exp> [ <sep> ]
; where at least one match of <exp> is guaranteed



; The boolean at the end determines whether an optional
; trailing separator is allowed.


; It also supports a special selective sequence form:

; #'(<qq-pat> ...) where <qq-pat> ::= #,<pat> | <pat>
;    is a selective sequence, where only elements
;    marked with #, are retained

; Ex: #'(#,foo bar #,baz) 
;     ==>
;     ($--> (seq foo bar baz) 
;           (list ($ 1) ($ 3)))



;; Helpers

; unzip/callback : (b c) list -> (b list c list -> ans) -> ans
; Unzips a list in continuation-passing style.
(define (unzip/callback lst k)
  (match lst
    ['() (k '() '())]
    [(cons (list a b) tl)
     (unzip/callback tl (λ (as bs)
       (k (cons a as) (cons b bs))))]))



; forces-$* : exp -> exp
; Forces the expression to be of the form ($--> (seq <exp> ...) <body>)
; Yacc-like tools need high-level rules to have this form.
(define (force-$* exp)
  (match exp
    [`($*--> ,pats ... ,body)
     ; =>
     (force-$* `($--> (seq ,@pats) ,body))]
    
    [`($--> (seq . ,exps) . ,body)
     ; =>
     exp]
    
    [`($--> ,pat . ,body)
     ; =>
     `($--> (seq ,pat) 
            (let ([$ (λ (n) (list-ref ($ 1) n))]
                  [$$ ($ 1)]) . ,body))]
    
    [`(seq . ,exps)
     ; =>
     `($--> ,exp $$)]
    
    [(or `(quote ,_)
         (? symbol?)
         (? string?)
         `(rep ,_)
         `(rep+ ,_)
         `(or . ,_))
     `($--> (seq ,exp) ($ 1))]))

; force-or : exp -> exp
; Forces the expression to be of the form (or <exp> ...)
; Yacc-like tools need the or at the top level.
(define (force-or exp)
  (match exp
    [`(or . ,exps) exp]
    [else `(or ,exp)]))




; desugar : grammar -> grammar
; Eliminates these desugarable constructs from the grammar:

; ($*--> <pat> ... <exp>)
; (>--> <pat> <match-clauses>)
; (@--> <pat> <exp>)
; (cons <pat> <pat>)
; (car <pat>)
; (cdr <pat>)

; (rep/sep <pat> <pat> <bool>)
; (rep/sep+ <pat> <pat> <bool>)
; #'(<qq-pat> ...) where <qq-pat> ::= #,<pat> | <pat>
(define (desugar grammar)
  
  
  (define added-rules '())
  
  (define (add-rule! non-term rhs)
    (set! added-rules (cons `[,non-term ,rhs] added-rules)))
  
  ; atomize! : exp -> exp
  ; Makes an expression a terminal on non-terminal, if it isn't one.
  (define (atomize! exp)
    (match exp

      ; Terminals:
      [`(quote ,(? symbol?))
       ;=>
       exp]
      
      [(? symbol?)
       ;=>
       exp]
      
      [(? string?)
       ;=>
       exp]
      
      [else
       ;=>
       (define $nt (gensym 'nt))
       (add-rule! $nt exp)
       $nt]))
  
  ; desugar-exp : exp -> exp
  ; Desugars an individual construct and its children.
  (define (desugar-exp exp)
    (match exp
    
      ; Terminals:
      [`(quote ,(? symbol?))
       ;=>
       exp]
      
      [(? symbol?)
       ;=>
       exp]
      
      [(? string?)
       ;=>
       exp]
      
      ; Reductions:
      [`($--> ,exp . ,body)
       `($--> ,(desugar-exp exp) . ,body)]
      
      [`(@--> ,exp ,fn)
       `($--> (seq ,exp) (apply ,fn ($ 1)))]

      [`(>--> ,exp . ,match-clauses)
       `($--> (seq ,exp) (match ($ 1) . ,match-clauses))]
      
      [`($*--> ,exps ... ,body)
       `($--> (seq ,@(map desugar-exp exps)) ,body)]
      
      [`(cons ,exp1 ,exp2)
       `($--> (seq ,(desugar-exp exp1) ,(desugar-exp exp2)) (cons ($ 1) ($ 2)))]
      
      [`(car ,exp)
       `($--> (seq ,(desugar-exp exp)) (car ($ 1)))]
      
      [`(cdr ,exp)
       `($--> (seq ,(desugar-exp exp)) (cdr ($ 1)))]
      
      [(or `(syntax ,args) `(quasisyntax ,args))
       ;=>
       (define (desugar-quoted exp position)
         (match exp
           [`(unsyntax ,exp)
            `(,position ,(desugar-exp exp))]
           
           [else 
            `(#f ,(desugar-exp exp))]))
         
       (define desugared (map desugar-quoted args (for/list ([i (in-range 1 (+ 1 (length args)))]) i)))
       
       (unzip/callback
        desugared 
        (λ (positions exps)
          (desugar-exp 
           `($--> (seq ,@exps)
                  (list ,@(map (λ (p) `($ ,p)) (filter identity positions)))))))]
      
      ; Core forms:
      [`(seq . ,exps)
       ; =>
       `(seq ,@(map desugar-exp exps))]
      
      [`(or . ,exps)
       ; =>
       `(or ,@(map desugar-exp exps))]
      
      
      ; Complex constructs:      
      [`(rep ,exp)
       ; =>
       `(rep ,(desugar-exp exp))]
      
      [`(rep+ ,exp)
       ; =>
       `(rep+ ,(desugar-exp exp))]

      ; rep+/sep matches <exp> <sep> ... <sep> <exp> [ <sep> ]
      ; (At least one <exp> match is guaranteed.)
      ; This is useful for constructs like comma-separated lists.
      [`(rep+/sep ,sep ,exp ,sep-tail?)
       ; => 

       (define $exp (atomize! (desugar-exp exp)))
       (define $sep (atomize! (desugar-exp sep)))
       
       (define $rep (gensym '$rep))
       
       (add-rule! $rep `(or ($--> (seq) '())
                            ($--> (seq ,$sep ,$exp ,$rep)
                                  (cons ($ 2) ($ 3)))))
       
       (define $tail (if sep-tail?
                         `((opt ,$sep))
                         `()))
        
       `(or 
            ($--> (seq ,$exp)       (list ($ 1)))
            ($--> (seq ,$exp ,$rep . ,$tail) (cons ($ 1) ($ 2))))]

      
      ; rep/sep matches <exp> <sep> ... <sep> <exp> [ <sep> ]
      ; (Zero or more matches of <exp> are allowed.)
      [`(rep/sep ,sep ,exp ,sep-tail?)
       
       (define $exp (atomize! (desugar-exp exp)))
       (define $sep (atomize! (desugar-exp sep)))
       
       (define $rep (gensym '$rep))
       
       (add-rule! $rep `(or ($--> (seq) '())
                            ($--> (seq ,$sep ,$exp ,$rep)
                                  (cons ($ 2) ($ 3)))))
       
       (define $tail (if sep-tail?
                         `((opt ,$sep))
                         `()))
        
       `(or ($--> (seq)            '())
            ($--> (seq ,$exp)       (list ($ 1)))
            ($--> (seq ,$exp ,$rep . ,$tail) (cons ($ 1) ($ 2))))]
            
            
      
      [`(opt ,exp ,value)
       ; =>
       `(opt ,(desugar-exp exp) ,value)]
      
      [`(opt ,exp)
       ; =>
       `(opt ,(desugar-exp exp))]
      
      
      [else 
       ; =>
       (error (format "could not desugar: ~a" exp))]))
  
  
  (define (desugar-rule rule)
    (match rule
      [`[,non-term ,rhs]
       `[,non-term ,(desugar-exp rhs)]]))
  
  (define transformed-rules (map desugar-rule grammar))
  
  (append transformed-rules added-rules))
  
  
;; compile-derp-rules : derp-grammar -> yacc-grammar
;; Transforms a derp-style grammar into a racket-style yacc grammar.
(define (compile-derp-rules rules)

  (define extra-rules '())
  
  (define (add-rule non-term exp)
    (set! extra-rules (cons `[,non-term ,exp] extra-rules)))
  
  ;; derp->yacc translation:
  
  ; wrapping makes $ and $$ available in the body:
  (define (wrap-$ size body)
    (define $$ (for/list ([i (in-range 1 (+ 1 size))]) `($ ,i)))
    `(let-syntax
         [($ (λ (stx)
               (syntax-case stx ()
                 [(_ n)  (datum->syntax
                          #'n 
                          (string->symbol 
                           (string-append
                            "$" (number->string
                                 (syntax->datum #'n)))))])))]
       (let-syntax 
           ([$$ (λ (_) #'(list ,@$$))])
         . ,body)))

  (define (translate-terminal str)
    (string->symbol str))
  
  (define (translate-abstract-terminal sym)
    (match sym
      ; Remove the extra layer of quoting used
      ; to denote classes of terminals from
      ; literal terminals, e.g. 'NUM versus :
      [`(quote ,(and aterm (? symbol?)))
       aterm]))
  
  (define (translate-exp exp)
    (match exp
      [(? string?)   (translate-terminal exp)]
      
      [(? symbol?)   exp]
      
      [`(quote ,(? symbol?))
       ;=>
       (translate-abstract-terminal exp)]))
  
  (define (translate-reduction reduction)
    (match reduction
      [`($--> (seq . ,exps) . ,body)
       (list (map translate-exp exps)
             (wrap-$ (length exps) body))]))
  
  (define (translate-rhs rhs)
    (match rhs
      [`(or . ,reductions) 
       (map translate-reduction reductions)]))
  
  (define (translate-rule rule)
    (match rule
      [`(,nonterm ,rhs)
       `(,nonterm ,@(translate-rhs rhs))]))
       
  
  ;; normalization ensures that all expressions on
  ;; right-hand sides are flattened.
  (define (normalize-rule rule)
    (match rule
      [`(,nonterm ,exp)
       ;=>
       `[,nonterm ,(normalize-rhs exp)]]))
  
  (define (normalize-rhs exp)
    (match (force-or exp)
      [`(or . ,exps)  
       ; =>
       `(or ,@(map (compose flatten-$* force-$*) exps))]))
      
  
  ;; flattening converts an expression in the grammar into
  ;; something suitable for a yacc/BNF-style grammar.
  
  ;; For example, consider a derp-style rule like:
  
  ; term ::= (seq factor (rep (seq "+" term)))
  
  ; this needs to flatten to:
  
  ; term   ::= (seq factor $rep1)
  ; $rep1  ::= (rep (seq "+" term))
  
  ; and then into:
  
  ; term   ::= (seq factor $rep1)
  ; $rep1  ::= (rep $seq1)
  ; $seq1  ::= (seq "+" term)
  
  ; and then into:
  
  ; term   ::= (seq factor $rep1)
  ; $rep1  ::= (seq $seq1 $rep1)
  ;         |  (seq)
  ; $seq1  ::= (seq "+" term)
  
  ; and, at this point, it is yacc/BNF-compatible.
    
  ; Technically, to keep the reductions the same, 
  ; it becomes equivalent to:
  
  ; term   ::= (seq factor $rep1)
  ; $rep1  ::= (cons $seq1 $rep1)
  ;         |  (seq)
  ; $seq1  ::= (seq "+" term)
  
  
  (define (flatten-$* exp)
    (match exp
      [`($--> (seq . , exps) . ,body)
       ;=>
       `($--> (seq ,@(map flatten-exp exps)) . ,body)]))
       
  ; flatten things that can't be a *sub*-expression:
  ; opt
  ; rep
  ; rep+ 
  ; seq
  ; $-->
  (define (flatten-exp exp)
    (match exp
      [`(seq . ,_)
       ;=>
       (define $nonterm (gensym '$nt))
       (add-rule $nonterm (normalize-rhs exp))
       $nonterm]
      
      [`($--> . ,_)
       ;=>
       (define $nonterm (gensym '$nt))
       (add-rule $nonterm (normalize-rhs exp))
       $nonterm]
      
      
      [`(or . ,_)
       ;=>
       (define $nonterm (gensym '$nt))
       (add-rule $nonterm (normalize-rhs exp))
       $nonterm]

      
      [`(opt ,exp ,default-value)
       ;=>
       (define $nonterm (gensym '$nt))
       (add-rule $nonterm `(or ($--> (seq ,(flatten-exp exp)) ($ 1))
                               ($--> (seq) ,default-value)))
       $nonterm]
      
      [`(opt ,exp)
       ;=>
       (flatten-exp `(opt ,exp #f))]
      
      [`(rep ,exp)
       ;=>
       (define $nonterm (gensym '$nt))
       (add-rule $nonterm `(or ($--> (seq  ,(flatten-exp exp) ,$nonterm) (cons ($ 1) ($ 2)))
                               ($--> (seq) '())))
       $nonterm]

      [`(rep+ ,exp)
       ;=>
       (define $nonterm (gensym '$nt))
       (define flattened-exp (flatten-exp exp))
       (add-rule $nonterm `(or ($--> (seq ,flattened-exp ,$nonterm) (cons ($ 1) ($ 2)))
                               ($--> (seq ,flattened-exp) (list ($ 1)))))
       $nonterm]


      
      [`(quote ,(and aterm (? symbol?)))
       ;=>
       exp]
      
      [(? symbol?)
       ;=>
       exp]
      
      [(? string?)
       ;=>
       ; Make literal terminals reduce to their own string value:
       (define $nonterm (gensym (string->symbol (string-append "$" exp))))
       (add-rule $nonterm `(or ($--> (seq ,exp) ,exp)))
       $nonterm]))
       

       
  (define old-rules (map normalize-rule rules))
  
  (define new-rules (append old-rules extra-rules))
  
  (map translate-rule new-rules))
  

(define grammar-input-file #f)
(define grammar-input-port #f)

(match (current-command-line-arguments)
  
  [(vector "--drracket")
   (set! grammar-input-file "default.grammar.sx")
   (set! grammar-input-port (open-input-file grammar-input-file))]
  
  [(vector file-name)
   (set! grammar-input-file file-name)
   (set! grammar-input-port (open-input-file grammar-input-file))]
  
  [(vector)
   (set! grammar-input-port (current-input-port))])



(define python-grammar (read grammar-input-port))


(define desugared-python-grammar (desugar python-grammar))

;(pretty-write desugared-python-grammar)

(define python-yacc-rules (compile-derp-rules desugared-python-grammar))


(define yacc-output-port (current-output-port))

(when grammar-input-file
  (define yacc-output-file (regexp-replace #rx"[.]grammar" grammar-input-file ".yacc"))
  (set! yacc-output-port (open-output-file yacc-output-file #:exists 'replace)))
  
  
(for ([rule python-yacc-rules])
  (pretty-write rule yacc-output-port)
  (newline yacc-output-port))

  


