#lang racket

(require rackunit)

(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

;; original definition
(define leftmost-o
  (lambda (l)
    (cond
      [(atom? (car l)) (car l)]
      [else (leftmost-o (car l))])))

(define leftmost-without-let
  (lambda (l)
    (cond
      [(null? l) '()]
      [(atom? (car l)) (car l)]
      [else (cond
              [(atom? (leftmost-without-let (car l))) (leftmost-without-let (car l))]
              [else (leftmost-without-let (cdr l))])])))

(check-equal? (leftmost-without-let '(((a) b) (c d))) 'a)


(define leftmost
  (lambda (l)
    (cond
      [(null? l) '()]
      [(atom? (car l)) (car l)]
      [else (let ([a (leftmost (car l))])
                  (cond
                    [(atom? a) a]
                    [else (leftmost (cdr l))]))])))

(check-equal? (leftmost '(((a) b) (c d))) 'a)

