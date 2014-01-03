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

(define rember-f
  (lambda (test? a lat)
    (cond
      [(null? lat) '()]
      [(test? a (car lat)) (cdr lat)]
      [else (cons (car lat) (rember-f test? a (cdr lat)))])))

(check-equal? (rember-f equal? 'mint '(lamb chops and mint jelly))
              '(lamb chops and jelly))
(check-equal? (rember-f equal? 'mint '(lamb and mint flavored mint jelly))
              '(lamb and flavored mint jelly))
(check-equal? (rember-f equal? 'mint '(egg sandwich))
              '(egg sandwich))
(check-equal? (rember-f equal? 'cup '(coffee cup tea cup and hick cup))
              '(coffee tea cup and hick cup))
(check-equal? (rember-f = 5 '(6 2 5 3)) '(6 2 3))
(check-equal? (rember-f equal? '(pop corn) '(lemonade (pop corn) and (cake)))
              '(lemonade and (cake)))

(define eq?-c
  (lambda (a)
    (lambda (x) (eq? x a))))

(define eq?-salad
  (eq?-c 'salad))

(check-true (eq?-salad 'salad))
(check-false (eq?-salad 'blah))

(check-true ((eq?-c 'salad) 'salad))
(check-false ((eq?-c 'salad) 'blah))

(define rember-f2
  (lambda (test?)
    (lambda (a l)
      (cond
        [(null? l) '()]
        [(test? a (car l)) (cdr l)]
        [else (cons (car l) ((rember-f2 test?) a (cdr l)))]))))

(define rember-eq?
  (rember-f2 eq?))

(check-equal? ((rember-f2 eq?) 'tuna '(tuna salad)) '(salad))
(check-equal? ((rember-f2 =) 1 '(1 2 3)) '(2 3))


(define insertL-f
  (lambda (test?)
    (lambda (new old lat)
      (cond
        [(null? lat) '()]
        [(test? old (car lat)) (cons new (cons old (cdr lat)))]
        [else (cons (car lat) ((insertL-f test?) new old (cdr lat)))]))))

(check-equal? ((insertL-f equal?) 'topping 'fudge '(ice cream with fudge for dessert))
              '(ice cream with topping fudge for dessert))
(check-equal? ((insertL-f equal?) 'e 'f '(a b c d f g h)) '(a b c d e f g h))
(check-equal? ((insertL-f =) 2 3 '(1 3)) '(1 2 3))


(define insertR-f
  (lambda (test?)
    (lambda (new old lat)
      (cond
        [(null? lat) '()]
        [(test? old (car lat)) (cons old (cons new (cdr lat)))]
        [else (cons (car lat) ((insertR-f test?) new old (cdr lat)))]))))

(check-equal? ((insertR-f equal?) 'new 'old '()) '())
(check-equal? ((insertR-f equal?) 'topping 'fudge '(ice cream with fudge for dessert fudge))
              '(ice cream with fudge topping for dessert fudge))
(check-equal? ((insertR-f equal?) 'e 'd '(a b c d f g h)) '(a b c d e f g h))
(check-equal? ((insertR-f =) 3 2 '(1 2)) '(1 2 3))

(define seqR
  (lambda (new old l)
    (cons old (cons new l))))

(define seqL
  (lambda (new old l)
    (cons new (cons old l))))

(define insert-g
  (lambda (seq)
      (lambda (new old l)
        (cond
          [(null? l) '()]
          [(eq? old (car l)) (seq new old (cdr l))]
          [else (cons (car l) ((insert-g seq) new old (cdr l)))]))))

(check-equal? ((insert-g seqR) 3 2 '(1 2)) '(1 2 3))

(define insertL (insert-g seqL))

(check-equal? (insertL 2 3 '(1 3)) '(1 2 3))

(define insertR (insert-g seqR))

(check-equal? (insertR 3 2 '(1 2)) '(1 2 3))

(define insertL-anonymous
  (insert-g
   (lambda (new old l)
     (cons new (cons old l)))))

(check-equal? (insertL-anonymous 2 3 '(1 3)) '(1 2 3))


(define subst
  (insert-g
   (lambda (new old l)
     (cons new l))))

(check-equal? (subst 'topping 'fudge '(ice cream with fudge for dessert))
              '(ice cream with topping for dessert))
(check-equal? (subst 'new 'old '()) '())
(check-equal? (subst 'e 'f '(a b c d f g h)) '(a b c d e g h))

(define rember
  (lambda (a l)
    ((insert-g
     (lambda (new old l)
       l)) #f a l)))

(check-equal? (rember 1 '(1 2)) '(2))


(define 1st-sub-exp
  (lambda (aexp)
    (car (cdr aexp))))

(define 2nd-sub-exp
  (lambda (aexp)
    (car (cdr (cdr aexp)))))

(define atom-to-function
  (lambda (x)
    (cond
      [(eq? x '+) plus]
      [(eq? x '*) mult]
      [else exp])))

;; value using the prefix notaion for numeric expressions
(define value
  (lambda (nexp)
    (cond
      [(atom? nexp) nexp]
      [else ((atom-to-function (car nexp))
            (value (1st-sub-exp nexp)) (value (2nd-sub-exp nexp)))])))

(check-equal? (value '14) '14)
(check-equal? (value '(+ 1 3)) '4)
(check-equal? (value '(+ 1 (^ 2 3))) '9)

(define multirember-f
  (lambda (test?)
    (lambda (a lat)
      (cond
        [(null? lat) '()]
        [(test? a (car lat)) ((multirember-f test?) a (cdr lat))]
        [else (cons (car lat) ((multirember-f test?) a (cdr lat)))]))))

(check-equal? ((multirember-f eq?) 'tuna '(shrimp salad tuna salad and tuna))
              '(shrimp salad salad and))

(define multirember-eq? (multirember-f eq?))

(define eq?-tuna
  (lambda (a)
    (eq?-c 'tuna)))


(define multirember&co
    (lambda (a lat col)
      (cond
        [(null? lat) (col '() '())]
        [(eq? a (car lat)) (multirember&co a
                                           (cdr lat)
                                           (lambda (newlat seen)
                                              (col newlat (cons (car lat) seen))))]
        [else (multirember&co a
                              (cdr lat)
                              (lambda (newlat seen)
                                (col (cons (car lat) newlat) seen)))])))

;; list of matches empty?
(check-false (multirember&co 'tuna '(tuna pasta) (lambda (x y) (null? y))))
(check-true (multirember&co 'tuna '(strawberry cheesecake) (lambda (x y) (null? y))))

;; count the number of matches
(check-equal? (multirember&co 'tuna '(tuna pasta and tuna salad)
                              (lambda (x y) (length y))) 2)


(define multiinsertLR
  (lambda (new oldL oldR lat)
    (cond
      [(null? lat) '()]
      [(eq? oldL (car lat))
       (cons new (cons (car lat) (multiinsertLR new oldL oldR (cdr lat))))]
      [(eq? oldR (car lat))
       (cons (car lat) (cons new (multiinsertLR new oldL oldR (cdr lat))))]
      [else (cons (car lat) (multiinsertLR new oldL oldR (cdr lat)))])))

(check-equal? (multiinsertLR 'test 'hello 'world '(hello world))
              '(test hello world test))
(check-equal? (multiinsertLR 'test 'hello 'world '(hello there world))
              '(test hello there world test))


(define multiinsertLR&co
  (lambda (new oldL oldR lat col)
    (cond
      [(null? lat) (col '() 0 0)]
      [(eq? oldL (car lat))
       (multiinsertLR&co new oldL oldR (cdr lat)
                         (lambda (newlat L R)
                           (col (cons new (cons (car lat) newlat)) (add1 L) R)))]
      [(eq? oldR (car lat))
       (multiinsertLR&co new oldL oldR (cdr lat)
                         (lambda (newlat L R)
                           (col (cons (car lat) (cons new newlat)) L (add1 R))))]
      [else (multiinsertLR&co new oldL oldR (cdr lat)
                              (lambda (newlat L R)
                                (col (cons (car lat) newlat) L R)))])))

(check-equal? (multiinsertLR&co 'test 'hello 'world '(hello world)
                             (lambda (newlat L R) (list newlat L R)))
              '((test hello world test) 1 1))
(check-equal? (multiinsertLR&co 'test 'hello 'worlds '(hello there world)
                             (lambda (newlat L R) (list newlat L R)))
              '((test hello there world) 1 0))
(check-equal? (multiinsertLR&co 'salty 'fish 'chips '(chips and fish or fish and chips)
                             (lambda (newlat L R) (list newlat L R)))
              '((chips salty and salty fish or salty fish and chips salty) 2 2))
