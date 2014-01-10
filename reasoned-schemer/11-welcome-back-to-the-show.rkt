#lang racket

(require rackunit)

(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

(define member?
  (lambda (a lat)
    (cond
      [(null? lat) #f]
      [(eq? a (car lat)) #t]
      [else (member? a (cdr lat))])))

(define two-in-a-row?
  (lambda (lat)
    (cond
      [(null? lat) #f]
      [(null? (cdr lat)) #f]
      [(eq? (car lat) (car (cdr lat))) #t]
      [else (two-in-a-row? (cdr lat))])))

(check-false (two-in-a-row? '(a b c)))
(check-true (two-in-a-row? '(a b b c)))
