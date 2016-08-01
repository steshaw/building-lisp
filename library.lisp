;;
;; List functions
;;

(define (foldl proc init list)
  (if list
    (foldl proc (proc init (car list))
           (cdr list))
    init))

(define (reduce proc list)
  (if (pair? list)
      (foldl proc (car list) (cdr list))
      nil))

(define (foldr proc init list)
  (if list
    (proc (car list)
          (foldr proc init (cdr list)))
    init))

(define (list . items)
  (foldr cons nil items))

(define (reverse list)
  (foldl (lambda (a x) (cons x a)) nil list))

(define (unary-map proc list)
  (foldr (lambda (x rest) (cons (proc x) rest))
         nil
         list))
(define (map proc . arg-lists)
  (if (car arg-lists)
    (cons (apply proc (unary-map car arg-lists))
          (apply map (cons proc (unary-map cdr arg-lists))))
    nil))

(define (append a b) (foldr cons b a))
(define (caar x) (car (car x)))
(define (cadr x) (car (cdr x)))

;;
;; Quasiquote
;;
(defmacro (quasiquote x)
  (if (pair? x)
      (if (eq? (car x) 'unquote)
          (cadr x)
          (if (eq? (caar x) 'unquote-splicing)
              (list 'append
                    (cadr (car x))
                    (list 'quasiquote (cdr x)))
              (list 'cons
                    (list 'quasiquote (car x))
                    (list 'quasiquote (cdr x)))))
      (list 'quote x)))

;;
;; Macros
;;

(defmacro (ignore x)
  `(quote ,x))

(defmacro (begin . body)
  `((lambda () ,@body)))

(defmacro (let defs . body)
  `((lambda ,(map car defs) ,@body)
    ,@(map cadr defs)))

;;
;; Integer functions
;;

(define +
  (let ((old+ +))
    (lambda xs (foldl old+ 0 xs))))

(define -
  (let ((old- -))
    (lambda (x . xs)
      (if xs
        (foldl old- x xs)
        (old- 0 x)))))

(define *
  (let ((old* *))
    (lambda xs (foldl old* 1 xs))))

(define /
  (let ((old/ /))
    (lambda (x . xs)
      (if xs
          (foldl old/ x xs)
          (old/ 1 x)))))

(define (abs x) (if (< x 0) (- 0 x) x))

(define (fact n) (if (= n 0) 1 (* n (fact (- n 1)))))

;;
;; functions to help thrash the GC.
;;

(define (count n)
  (if (= n 0)
      0
      (+ 1 (count (- n 1)))))

(define (count-tail n)
  (define (count-tail-aux n a)
    (if (= n 0)
        a
        (count-tail-aux (- n 1) (+ a 1))))
  (count-tail-aux n 0))

(define (thrash n)
  (if (= n 0)
      nil
      (begin (list 1 2 3 4 5 6 7 8 9 0)
             (thrash (- n 1)))))
