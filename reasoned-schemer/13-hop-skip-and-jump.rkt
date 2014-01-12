#lang racket

(require rackunit)

(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

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

(define intersect-o
  (lambda (set1 set2)
    (cond
      [(null? set1) '()]
      [(member? (car set1) set2) (cons (car set1) (intersect-o (cdr set1) set2))]
      [else (intersect-o (cdr set1) set2)])))

(check-equal? (intersect-o '(a b c) '(c)) '(c))

(define intersect
  (lambda (set1 set2)
    (letrec
        ([I (lambda (set)
              (cond
                [(null? set) '()]
                [(member? (car set) set2) (cons (car set) (I (cdr set)))]
                [else (I (cdr set))]))])
      (I set1))))

(check-equal? (intersect '(a b c) '(c)) '(c))

;; original definition
(define intersectall-o
  (lambda (lset)
    (cond
      [(null? lset) '()]
      [(null? (cdr lset)) (car lset)]
      [else (intersect (car lset) (intersectall-o (cdr lset)))])))

(define intersectall-letrec
  (lambda (lset)
    (letrec
        ([A (lambda (lset)
                         (cond
                           [(null? (cdr lset)) (car lset)]
                           [else (intersect (car lset) (A (cdr lset)))]))])
    (cond
      [(null? lset) '()]
      [else (A lset)]))))

(check-equal? (intersectall-letrec '((a b) (a) (c a))) '(a))
(check-equal? (intersectall-letrec '((3 mangos and)
                              (3 kiwis and)
                              (3 hamburgers))) '(3))
(check-equal? (intersectall-letrec '((3 mangos and)
                              ()
                              (3 hamburgers))) '())

(define intersectall-cc
  (lambda (lset)
    (call/cc
     (lambda (hop)
       (letrec
           ([A (lambda (lset)
                 (cond
                   [(null? (car lset)) (hop '())]
                   [(null? (cdr lset)) (car lset)]
                   [else (intersect (car lset) (A (cdr lset)))]))])
         (cond
           [(null? lset) '()]
           [else (A lset)]))))))

(check-equal? (intersectall-cc '((a b) (a) (c a))) '(a))
(check-equal? (intersectall-cc '((3 mangos and)
                                      (3 kiwis and)
                                      (3 hamburgers))) '(3))
(check-equal? (intersectall-cc '((3 mangos and)
                                      ()
                                      (3 hamburgers))) '())

(define intersectall
  (lambda (lset)
    (call/cc
     (lambda (hop)
       (letrec
           ([A (lambda (lset)
                 (cond
                   [(null? (car lset)) (hop '())]
                   [(null? (cdr lset)) (car lset)]
                   [else (intersect (car lset) (A (cdr lset)))]))]
            [I (lambda (s1 s2)
                 (letrec
                     ([J (lambda (set)
                           (cond
                             [(null? set) '()]
                             [(member? (car set) s2) (cons (car set) (J (cdr set)))]
                             [else (J (cdr set))]))])
                   (cond
                     [(null? s2) (hop '())]
                     [else (J s1)])))])
         (cond
           [(null? lset) '()]
           [else (A lset)]))))))

(check-equal? (intersectall '((a b) (a) (c a))) '(a))
(check-equal? (intersectall '((3 mangos and)
                              (3 kiwis and)
                              (3 hamburgers))) '(3))
(check-equal? (intersectall '((3 mangos and)
                              ()
                              (3 hamburgers))) '())

(define rember
  (lambda (a lat)
    (letrec
        ([R (lambda (lat)
              (cond
                [(null? lat) '()]
                [(eq? (car lat) a) (cdr lat)]
                [else (cons (car lat) (R (cdr lat)))]))])
      (R lat))))

(check-equal? (rember 'a '(a b c)) '(b c))

(define rember-beyond-first
  (lambda (a lat)
    (letrec
        ([R (lambda (lat)
              (cond
                [(null? lat) '()]
                [(eq? (car lat) a) '()]
                [else (cons (car lat) (R (cdr lat)))]))])
      (R lat))))

(check-equal? (rember-beyond-first 'b '(a b c)) '(a))


(define rember-upto-last
  (lambda (a lat)
    (call/cc
     (lambda (skip)
       (letrec
           ([R (lambda (lat)
                 (cond
                   [(null? lat) '()]
                   [(eq? (car lat) a) (skip (R (cdr lat)))]
                   [else (cons (car lat) (R (cdr lat)))]))])
         (R lat))))))

(check-equal? (rember-upto-last 'b '(a b c)) '(c))
