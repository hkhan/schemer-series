#lang racket

(require rackunit)

(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

(check-true (lat? '(hello world)))
(check-false (lat? '((hello) world)))
(check-true (lat? '()))

(define lat?
  (lambda (l)
    (cond
      [(null? l) #t]
      [(atom? (car l)) (lat? (cdr l))]
      [else #f])))


(check-true (member? 'hello '(hello world)))
(check-false (member? 'test '(hello world)))
(check-false (member? 'hello '()))

(define member?
  (lambda (a lat)
    (cond
      [(null? lat) #f]
      [else (or (eq? a (car lat)) (member? a (cdr lat)))])
  ))


(check-equal? (rember 'mint '(lamb chops and mint jelly)) 
              '(lamb chops and jelly))
(check-equal? (rember 'mint '(lamb and mint flavored mint jelly)) 
              '(lamb and flavored mint jelly))
(check-equal? (rember 'mint '(egg sandwich)) 
              '(egg sandwich))
(check-equal? (rember 'cup '(coffee cup tea cup and hick cup)) 
              '(coffee tea cup and hick cup))

(define rember
  (lambda (a lat)
    (cond
      [(null? lat) '()]
      [(eq? a (car lat)) (cdr lat)]
      [else (cons (car lat) (rember a (cdr lat)))])))


(check-equal? (firsts '((apple peach pumpkin)
                        (plum pear cherry)
                        (grape raisin pea)
                        (bean carrot eggplant))) '(apple plum grape bean))
(check-equal? (firsts '()) '())
(check-equal? (firsts '(((five plums) four)
                        (eleven green oranges)
                        ((no) more))) '((five plums) eleven (no)))

(define firsts
  (lambda (l)
    (cond
      [(null? l) '()]
      [else (cons (car (car l)) (firsts (cdr l)))])))


(check-equal? (insertR 'new 'old '()) '())
(check-equal? (insertR 'topping 'fudge '(ice cream with fudge for dessert fudge))
              '(ice cream with fudge topping for dessert fudge))
(check-equal? (insertR 'e 'd '(a b c d f g h)) '(a b c d e f g h))

(define insertR
  (lambda (new old lat)
    (cond
      [(null? lat) '()]
      [(eq? old (car lat)) (cons old (cons new (cdr lat)))]
      [else (cons (car lat) (insertR new old (cdr lat)))]
    )))




(check-equal? (insertL 'topping 'fudge '(ice cream with fudge for dessert))
              '(ice cream with topping fudge for dessert))
(check-equal? (insertL 'e 'f '(a b c d f g h)) '(a b c d e f g h))

(define insertL
  (lambda (new old lat)
    (cond
      [(null? lat) '()]
      [(eq? old (car lat)) (cons new lat)]
      [else (cons (car lat) (insertL new old (cdr lat)))]
    )))


(check-equal? (subst 'topping 'fudge '(ice cream with fudge for dessert))
              '(ice cream with topping for dessert))
(check-equal? (subst 'new 'old '()) '())
(check-equal? (subst 'e 'f '(a b c d f g h)) '(a b c d e g h))

(define subst
  (lambda (new old lat)
    (cond
      [(null? lat) '()]
      [(eq? (car lat) old) (cons new (cdr lat))]
      [else (cons (car lat) (subst new old (cdr lat)))]
    )))
                    

(check-equal? (subst2 'vanilla 'choc 'banana '(the banana ice cream with choc topping))
              '(the vanilla ice cream with choc topping))
(check-equal? (subst2 'vanilla 'choc 'banana '(banana ice cream with choc topping))
              '(vanilla ice cream with choc topping))
(check-equal? (subst2 'new 'old1 'old2 '()) '())
(define subst2
  (lambda (new o1 o2 lat)
    (cond
      [(null? lat) '()]
      [(or (eq? (car lat) o1) (eq? (car lat) o2)) (cons new (cdr lat))]
      [else (cons (car lat) (subst2 new o1 o2 (cdr lat)))]
    )))


(check-equal? (multirember 'test '()) '())
(check-equal? (multirember 'cup '(coffee cup tea cup and hick cup))
              '(coffee tea and hick))

(define multirember
  (lambda (a lat)
    (cond
      [(null? lat) '()]
      [(eq? a (car lat)) (multirember a (cdr lat))]
      [else (cons (car lat) (multirember a (cdr lat)))]
      )))
                             

;; function which takes 3 args: atoms 'new' and 'old' and a lat.
;; returns a lat with 'new' inserted to the right of the ALL occurence of 'old'
(define multiinsertR
  (lambda (new old lat)
    (cond
      [(null? lat) '()]
      [(eq? old (car lat)) (cons old (cons new (multiinsertR new old (cdr lat))))]
      [else (cons (car lat) (multiinsertR new old (cdr lat)))]
    )))

(run-tests
 (test-suite
  "tests for multiinsertR"
  (check-equal? (multiinsertR 'new 'old '()) '())
  (check-equal? (multiinsertR 'topping 'fudge '(ice cream with fudge for dessert fudge))
                '(ice cream with fudge topping for dessert fudge topping))
  (check-equal? (multiinsertR 'e 'd '(a b c d f g h)) '(a b c d e f g h))))

;; function which takes 3 args: atoms 'new' and 'old' and a lat.
;; returns a lat with 'new' inserted to the left of the ALL occurence of 'old'
(define multiinsertL
  (lambda (new old lat)
    (cond
      [(null? lat) '()]
      [(eq? old (car lat)) (cons new (cons old (multiinsertL new old (cdr lat))))]
      [else (cons (car lat) (multiinsertL new old (cdr lat)))]
    )))

(run-tests
 (test-suite
  "tests for multiinsertL"
  (check-equal? (multiinsertL 'new 'old '()) '())
  (check-equal? (multiinsertL 'topping 'fudge '(ice cream with fudge for dessert fudge))
                '(ice cream with topping fudge for dessert topping fudge))
  (check-equal? (multiinsertL 'd 'e '(a b c e f g h)) '(a b c d e f g h))))

;; function which takes 3 args: atoms 'new' and 'old' and a lat.
;; returns a lat with ALL occurence of old replaced with new
(define multisubst
  (lambda (new old lat)
    (cond
      [(null? lat) '()]
      [(eq? (car lat) old) (cons new (multisubst new old (cdr lat)))]
      [else (cons (car lat) (multisubst new old (cdr lat)))]
    )))

(run-tests
 (test-suite
  "tests for multisubst"
  (check-equal? (multisubst 'topping 'fudge '(ice cream with fudge for dessert fudge))
                '(ice cream with topping for dessert topping))
  (check-equal? (multisubst 'new 'old '()) '())
  (check-equal? (multisubst 'e 'f '(a b c d f g h)) '(a b c d e g h))))



