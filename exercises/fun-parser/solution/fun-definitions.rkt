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


