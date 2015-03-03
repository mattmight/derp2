(define (process-binops base ops)
  (match ops 
    ['()
     base]

    [(cons (list op exp) rest)
     (process-binops `(,(string->symbol op) ,base ,exp) rest)]))

