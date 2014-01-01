#lang racket

(require rackunit)

(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))


(define add1
  (lambda (n)
    (+ n 1)))


(define sub1
  (lambda (n)
    (- n 1)))


(define plus
  (lambda (a b)
    (cond
      [(zero? b) a]
      [else (add1 (plus a (sub1 b)))])))

(check-equal? (plus 0 0) 0)
(check-equal? (plus 5 0) 5)
(check-equal? (plus 4 5) 9)


(define sub
  (lambda (a b)
    (cond
      [(zero? b) a]
      [else (sub1 (sub a (sub1 b)))])))

(check-equal? (sub 0 0) 0)
(check-equal? (sub 14 3) 11)
(check-equal? (sub 17 9) 8)


(define addtup
  (lambda (tup)
    (cond
      [(null? tup) 0]
      [else (plus (car tup) (addtup (cdr tup)))]
      )))

(check-equal? (addtup '()) 0)
(check-equal? (addtup '(1 2 3)) 6)
(check-equal? (addtup '(0 5)) 5)


(define mult
  (lambda (a b)
    (cond
      [(zero? b) 0]
      [else (plus a (mult a (sub1 b)))]
      )))

(check-equal? (mult 0 0) 0)
(check-equal? (mult 5 3) 15)
(check-equal? (mult 0 4) 0)


(define tup+
  (lambda (tup1 tup2)
    (cond
      [(null? tup1) tup2]
      [(null? tup2) tup1] 
      [else (cons (plus (car tup1) (car tup2)) (tup+ (cdr tup1) (cdr tup2)))]))) 

(check-equal? (tup+ '() '()) '())
(check-equal? (tup+ '(0 1 2 0) '(0 0 2 5)) '(0 1 4 5))
(check-equal? (tup+ '(1 2) '(3 4 5)) '(4 6 5))


(define >
  (lambda (n m)
    (cond
      [(zero? n) #f]
      [(zero? m) #t]
      [else (> (sub1 n) (sub1 m))])))

(check-true (> 2 1))
(check-true (> 3 0))
(check-false (> 3 3))
(check-false (> 0 3))
(check-false (> 0 0))
(check-false (> 1 3))


(define <
  (lambda (n m)
    (cond
      [(zero? m) #f]
      [(zero? n) #t]
      [else (< (sub1 n) (sub1 m))])))

(check-true (< 1 2))
(check-true (< 0 3))
(check-false (< 3 3))
(check-false (< 3 0))
(check-false (< 0 0))
(check-false (< 3 1))


(define =
  (lambda (n m)
    (cond
      [(or (> n m) (> m n)) #f]
      [else #t])))

(check-true (= 0 0))
(check-true (= 1 1))
(check-false (= 1 2))
(check-false (= 2 1))


(define exp
  (lambda (n m)
    (cond
      [(zero? m) 1]
      [else (mult n (exp n (sub1 m)))])))

(check-equal? (exp 2 0) 1)
(check-equal? (exp 1 1) 1)
(check-equal? (exp 2 3) 8)


(define quotient
  (lambda (n m)
    (cond
      [(< n m) 0]
      [else (add1 (quotient (sub n m) m))])))

(check-equal? (quotient 1 1) 1)
(check-equal? (quotient 8 2) 4)
(check-equal? (quotient 8 3) 2)


(define length
  (lambda (lat)
    (cond
      [(null? lat) 0]
      [else (add1 (length (cdr lat)))])))

(check-equal? (length '()) 0)
(check-equal? (length '(test)) 1)
(check-equal? (length '(hello world)) 2)


(define pick
  (lambda (n lat)
    (cond
      [(zero? (sub1 n)) (car lat)]
      [else (pick (sub1 n) (cdr lat))])))

(check-equal? (pick 1 '(test)) 'test)
(check-equal? (pick 2 '(hello world)) 'world)


(define rempick
  (lambda (n lat)
    (cond
      [(zero? (sub1 n)) (cdr lat)]
      [else (cons (car lat) (rempick (sub1 n) (cdr lat)))])))

(check-equal? (rempick 1 '(test)) '())
(check-equal? (rempick 2 '(hello there world)) '(hello world))


(define no-nums
  (lambda (lat)
    (cond
      [(null? lat) '()]
      [(number? (car lat)) (no-nums (cdr lat))]
      [else (cons (car lat) (no-nums (cdr lat)))])))

(check-equal? (no-nums '()) '())
(check-equal? (no-nums '(1 2)) '())
(check-equal? (no-nums '(hello world)) '(hello world))
(check-equal? (no-nums '(1 hello 2 world 3)) '(hello world))


(define all-nums
  (lambda (lat)
    (cond
      [(null? lat) '()]
      [(number? (car lat)) (cons (car lat) (all-nums (cdr lat)))]
      [else (all-nums (cdr lat))])))

(check-equal? (all-nums '()) '())
(check-equal? (all-nums '(1 2)) '(1 2))
(check-equal? (all-nums '(hello world)) '())
(check-equal? (all-nums '(1 hello 2 world 3)) '(1 2 3))


(define equan?
  (lambda (a1 a2)
    (cond
      [(and (number? a1) (number? a2)) (= a1 a2)]
      [(and (not (number? a1)) (not (number? a2))) (eq? a1 a2)]
      [else #f])))

(check-true (equan? 1 1))
(check-true (equan? 'hello 'hello))
(check-false (equan? ''1 1))
(check-false (equan? 'hello 'world))


(define occur
  (lambda (a lat)
    (cond
      [(null? lat) 0]
      [(eq? a (car lat)) (add1 (occur a (cdr lat)))]
      [else (occur a (cdr lat))])))

(check-equal? (occur 1 '(1 2 3)) 1)
(check-equal? (occur 'hello '(hello world hello 1)) 2)
(check-equal? (occur 'hello '(world 1)) 0)
(check-equal? (occur 'hello '()) 0)


(define one?
  (lambda (n)
    (= n 1)))

(check-true (one? 1))
(check-false (one? 2))
(check-false (one? 0))


(define rempick-rewrite
  (lambda (n lat)
    (cond
      [(one? n) (cdr lat)]
      [else (cons (car lat) (rempick-rewrite (sub1 n) (cdr lat)))])))

(check-equal? (rempick-rewrite 1 '(test)) '())
(check-equal? (rempick-rewrite 2 '(hello there world)) '(hello world))



