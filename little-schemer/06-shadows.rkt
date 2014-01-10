#lang racket

(require rackunit)

(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

;; definitions from earlier exercises
(define plus
  (lambda (a b)
    (cond
      [(zero? b) a]
      [else (add1 (plus a (sub1 b)))])))

(define mult
  (lambda (a b)
    (cond
      [(zero? b) 0]
      [else (plus a (mult a (sub1 b)))])))

(define exp
  (lambda (n m)
    (cond
      [(zero? m) 1]
      [else (mult n (exp n (sub1 m)))])))
;;-------------------------------------------

(define numbered?
  (lambda (aexp)
    (cond
      [(atom? aexp) (number? aexp)]
      [(eq? (car (cdr aexp)) '+) (and (numbered? (car aexp))
                                       (numbered? (car (cdr (cdr aexp)))))]
      [(eq? (car (cdr aexp)) '*) (and (numbered? (car aexp))
                                       (numbered? (car (cdr (cdr aexp)))))]
      [(eq? (car (cdr aexp)) '^) (and (numbered? (car aexp))
                                       (numbered? (car (cdr (cdr aexp)))))]
      [else #f])))

(check-true (numbered? '3))
(check-false (numbered? 'test))
(check-true (numbered? '(3 + (4 * 5))))
(check-true (numbered? '(3 + (4 ^ 5))))
(check-false (numbered? '(2 * sausage)))

(define numbered-simplified?
  (lambda (aexp)
    (cond
      [(atom? aexp) (number? aexp)]
      [else (and (numbered? (car aexp)) (numbered? (car (cdr (cdr aexp)))))])))

(check-true (numbered-simplified? '3))
(check-false (numbered-simplified? 'test))
(check-true (numbered-simplified? '(3 + (4 * 5))))
(check-true (numbered-simplified? '(3 + (4 ^ 5))))
(check-false (numbered-simplified? '(2 * sausage)))

(define value
  (lambda (nexp)
    (cond
      [(atom? nexp) nexp]
      [(eq? (car (cdr nexp)) '+)
       (plus (value (car nexp)) (value (car (cdr (cdr nexp)))))]
      [(eq? (car (cdr nexp)) '*)
       (mult (value (car nexp)) (value (car (cdr (cdr nexp)))))]
      [else (exp (value (car nexp)) (value (car (cdr (cdr nexp)))))])))

(check-equal? (value '14) '14)
(check-equal? (value '(1 + 3)) '4)
(check-equal? (value '(1 + (2 ^ 3))) '9)

;; value using the prefix notaion for numeric expressions
(define valuep
  (lambda (nexp)
    (cond
      [(atom? nexp) nexp]
      [(eq? (car nexp) '+)
       (plus (valuep (car (cdr nexp))) (valuep (car (cdr (cdr nexp)))))]
      [(eq? (car nexp) '*)
       (mult (valuep (car (cdr nexp))) (valuep (car (cdr (cdr nexp)))))]
      [else (exp (valuep (car (cdr nexp))) (valuep (car (cdr (cdr nexp)))))])))

(check-equal? (valuep '14) '14)
(check-equal? (valuep '(+ 1 3)) '4)
(check-equal? (valuep '(+ 1 (^ 2 3))) '9)

(define 1st-sub-exp
  (lambda (aexp)
    (car (cdr aexp))))

(check-equal? (1st-sub-exp '(+ (+ 1 2) (+ 3 4))) '(+ 1 2))
(check-equal? (1st-sub-exp '(+ 1 2)) '1)

(define 2nd-sub-exp
  (lambda (aexp)
    (car (cdr (cdr aexp)))))

(check-equal? (2nd-sub-exp '(+ (+ 1 2) (+ 3 4))) '(+ 3 4))
(check-equal? (2nd-sub-exp '(+ 1 2)) '2)

;; value using the prefix notaion for numeric expressions
(define value2
  (lambda (nexp)
    (cond
      [(atom? nexp) nexp]
      [(eq? (car nexp) '+)
       (plus (value2 (1st-sub-exp nexp)) (value2 (2nd-sub-exp nexp)))]
      [(eq? (car nexp) '*)
       (mult (value2 (1st-sub-exp nexp) (value2 (2nd-sub-exp nexp))))]
      [else (exp (value2 (1st-sub-exp nexp)) (value2 (2nd-sub-exp nexp)))])))

(check-equal? (value2 '14) '14)
(check-equal? (value2 '(+ 1 3)) '4)
(check-equal? (value2 '(+ 1 (^ 2 3))) '9)

(define sero?
  (lambda (n)
    (null? n)))

(define edd1
  (lambda (n)
    (cons '() n)))

(define zub1
  (lambda (n)
    (cdr n)))
