(define (abs x) (if (< x 0) (- 0 x) x))

(define (foldl proc init list)
  (if list
    (foldl proc (proc init (car list))
           (cdr list))
    init))

(define (foldr proc init list)
  (if list
    (proc (car list)
          (foldr proc init (cdr list)))
    init))
