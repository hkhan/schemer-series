#lang racket

(require rackunit)

(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

(define multirember
  (lambda (a lat)
    (letrec
         ([mr (lambda (lat)
              (cond
                [(null? lat) '()]
                [(eq? a (car lat)) (mr (cdr lat))]
                [else (cons (car lat) (mr (cdr lat)))]))])
       (mr lat))))

(check-equal? (multirember 'b '(a b b c d)) '(a c d))

(define multirember-f
  (lambda (test?)
    (lambda (a lat)
      (cond
        [(null? lat) '()]
        [(test? a (car lat)) ((multirember-f test?) a (cdr lat))]
        [else (cons (car lat) ((multirember-f test?) a (cdr lat)))]))))

(check-equal? ((multirember-f eq?) 'b '(a b b c d)) '(a c d))

(define multirember-f-with-let
  (lambda (test?)
    (letrec
        ([m-f
          (lambda (a lat)
            (cond
              [(null? lat) '()]
              [(test? a (car lat)) (m-f a (cdr lat))]
              [else (cons (car lat) (m-f a (cdr lat)))]))])
      m-f)))

(check-equal? ((multirember-f-with-let eq?) 'b '(a b b c d)) '(a c d))

(define member-old?
  (lambda (a lat)
    (cond
      [(null? lat) #f]
      [(eq? a (car lat)) #t]
      [else (member-old? a (cdr lat))])))

(define member?
  (lambda (a lat)
    ((letrec
         ([yes? (lambda (l)
                  (cond
                    [(null? l) #f]
                    [(eq? a (car l)) #t]
                    [else (yes? (cdr l))]))])
       yes?)
     lat)))

(check-true (member? 'b '(c b d)))

;; definition without letrec i.e where set2 does not change during recursive calls
(define union-o
  (lambda (set1 set2)
    (cond
      [(null? set1) set2]
      [(member? (car set1) set2) (union-o (cdr set1) set2)]
      [else (cons (car set1) (union-o (cdr set1) set2))])))

(check-equal? (union-o '(a b) '(b c)) '(a b c))

(define union-with-let
  (lambda (set1 set2)
    (letrec
        ([U (lambda (set)
              (cond
                [(null? set) set2]
                [(member? (car set) set2) (U (cdr set))]
                [else (cons (car set) (U (cdr set)))]))])
      (U set1))))

(check-equal? (union-with-let '(a b) '(b c)) '(a b c))

(define union-with-M
  (lambda (set1 set2)
    (letrec
        ([U (lambda (set)
              (cond
                [(null? set) set2]
                [(M? (car set) set2) (U (cdr set))]
                [else (cons (car set) (U (cdr set)))]))]
         [M? (lambda (a lat)
               (cond
                 [(null? lat) #f]
                 [(eq? a (car lat)) #t]
                 [else (M? a (cdr lat))]))])
      (U set1))))

(check-equal? (union-with-M '(a b) '(b c)) '(a b c))

(define union
  (lambda (set1 set2)
    (letrec
        ([U (lambda (set)
              (cond
                [(null? set) set2]
                [(M? (car set) set2) (U (cdr set))]
                [else (cons (car set) (U (cdr set)))]))]
         [M? (lambda (a lat)
               (letrec ([N? (lambda (lat)
                              (cond
                                [(null? lat) #f]
                                [(eq? a (car lat)) #t]
                                [else (M? a (cdr lat))]))])
                 (N? lat)))])
      (U set1))))

(check-equal? (union '(a b) '(b c)) '(a b c))

(define two-in-a-row?
  (letrec
      ([W (lambda (a lat)
            (cond
              [(null? lat) #f]
              [else (or (eq? a (car lat)) 
                        (W (car lat) (cdr lat)))]))])
      (lambda (lat)
        (cond
          [(null? lat) #f]
          [else (W (car lat) (cdr lat))]))))

(check-false (two-in-a-row? '(a b c)))
(check-true (two-in-a-row? '(a b b c)))

(define sum-of-prefixes
  (lambda (tup)
    (letrec
        ([S (lambda (sonssf tup)
              (cond
                [(null? tup) '()]
                [else (cons (+ sonssf (car tup))
                            (S (+ sonssf (car tup)) (cdr tup)))]))])
      (S 0 tup))))

(check-equal? (sum-of-prefixes '(2 1 9 17 0)) '(2 3 12 29 29))
(check-equal? (sum-of-prefixes '(1 1 1 1 1)) '(1 2 3 4 5))


(define scramble
  (lambda (tup)
    (letrec
        ([P (lambda (tup rev-pre)
              (cond
                [(null? tup) '()]
                [else (cons (pick (car tup) (cons (car tup) rev-pre))
                            (P (cdr tup) (cons (car tup) rev-pre)))]))])
    (P tup (cons tup '())))))

(define pick
  (lambda (n lat)
    (cond
      [(= 1 n) (car lat)]
      [else (pick (sub1 n) (cdr lat))])))

(check-equal? (scramble '(1 1 1 3 4 2 1 1 9 2)) '(1 1 1 1 1 4 1 1 1 9))


      
     
                     




  