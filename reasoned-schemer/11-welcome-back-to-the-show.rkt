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
      [(is-first? (car lat) (cdr lat))]
      [else (two-in-a-row? (cdr lat))])))

;; this function could return false in 2 cases and it only makes sense to carry
;; on searching in the rest of the list in the second one of those.
(define is-first?
  (lambda (a lat)
    (cond
      [(null? lat) #f]
      [else (eq? a (car lat))])))

(check-false (two-in-a-row? '(a b c)))
(check-true (two-in-a-row? '(a b b c)))

;; we could leave the decision (about continuing to search) in is-firsts? function
(define two-in-a-row-revised?
  (lambda (lat)
    (cond
      [(null? lat) #f]
      [else (is-first-b? (car lat) (cdr lat))])))

(define is-first-b?
  (lambda (a lat)
    (cond
      [(null? lat) #f]
      [else (or (eq? a (car lat)) (two-in-a-row-revised? lat))])))

(check-false (two-in-a-row-revised? '(a b c)))
(check-true (two-in-a-row-revised? '(a b b c)))

;; we don't need to call back to two-in-row-revised as all that does is call back here.
;; so replace that with a recursive call to two-in-a-row-b?
(define two-in-a-row-b?
  (lambda (preceeding lat)
    (cond
      [(null? lat) #f]
      [else (or (eq? preceeding (car lat)) 
                (two-in-a-row-b? (car lat) (cdr lat)))])))

(define two-in-a-row-final?
  (lambda (lat)
    (cond
      [(null? lat) #f]
      [else (two-in-a-row-b? (car lat) (cdr lat))])))

(check-false (two-in-a-row-final? '(a b c)))
(check-true (two-in-a-row-final? '(a b b c)))

(define sum-of-prefixes
  (lambda (tup)
    (sum-of-prefixes-b 0 tup)))

;; sonssf = sum of number seen so far
(define sum-of-prefixes-b
  (lambda (sonssf tup)
    (cond
      [(null? tup) '()]
      [else (cons (+ sonssf (car tup))
                  (sum-of-prefixes-b (+ sonssf (car tup)) (cdr tup)))])))

(check-equal? (sum-of-prefixes '(2 1 9 17 0)) '(2 3 12 29 29))
(check-equal? (sum-of-prefixes '(1 1 1 1 1)) '(1 2 3 4 5))

;; function scramble takes a non-empty tup in which no number is greater than its
;; own index and returns a tup of the same length. Each number in the argument
;; is treated as a backward index from its own position to a point earlier in tup.
;; The result is found by counting backward from the current position according to 
;; this index
(define scramble
  (lambda (tup)
    (scramble-b tup (cons tup '()))))

(define scramble-b
  (lambda (tup rev-pre)
    (cond
      [(null? tup) '()]
      [else (cons (pick (car tup) (cons (car tup) rev-pre))
                  (scramble-b (cdr tup) (cons (car tup) rev-pre)))])))

(define pick
  (lambda (n lat)
    (cond
      [(= 1 n) (car lat)]
      [else (pick (sub1 n) (cdr lat))])))

(check-equal? (scramble '(1 1 1 3 4 2 1 1 9 2)) '(1 1 1 1 1 4 1 1 1 9))