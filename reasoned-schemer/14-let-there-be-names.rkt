#lang racket

(require rackunit)

(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

(define eqlist?
  (lambda (l1 l2)
    (cond
      [(and (null? l1) (null? l2)) #t]
      [(or (null? l1) (null? l2)) #f]
      [(and (atom? (car l1)) (atom? (car l2)))
            (and (equan? (car l1) (car l2)) (eqlist? (cdr l1) (cdr l2)))]
      [(or (atom? (car l1)) (atom? (car l2))) #f]
      [else (and (eqlist? (car l1) (car l2)) (eqlist? (cdr l1) (cdr l2)))])))

(define equan?
  (lambda (a1 a2)
    (cond
      [(and (number? a1) (number? a2)) (= a1 a2)]
      [(or (number? a1) (number? a2)) #f]
      [else (eq? a1 a2)])))

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


(define rember1*-default
  (lambda (a l)
    (cond
      [(null? l) '()]
      [(atom? (car l))
       (cond
         [(eq? a (car l)) (cdr l)]
         [else (cons (car l) (rember1*-default a (cdr l)))])]
      [else (cond
              [(eqlist? (rember1*-default a (car l)) (car l))
               (cons (car l) (rember1*-default a (cdr l)))]
              [else (cons (rember1*-default a (car l)) (cdr l))])])))

(check-equal? (rember1*-default 'b '((a b) a (c b d) b d)) '((a) a (c b d) b d))

; definition using letrec and rec
(define rember1*
  (lambda (a l)
    (letrec
        ([R (lambda (l)
              (cond
                [(null? l) '()]
                [(atom? (car l))
                 (cond
                   [(eq? a (car l)) (cdr l)]
                   [else (cons (car l) (R (cdr l)))])]
                [else (let ([av (R (car l))])
                        (cond
                          [(eqlist? av (car l)) (cons (car l) (R (cdr l)))]
                          [else (cons av (cdr l))]))]))])
      (R l))))

(check-equal? (rember1* 'b '((a b) a (c b d) b d)) '((a) a (c b d) b d))

