#lang racket

(require rackunit)

(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

;; from previous exercises
(define pick
  (lambda (n lat)
    (cond
      [(zero? (sub1 n)) (car lat)]
      [else (pick (sub1 n) (cdr lat))])))

(define first
  (lambda (p)
    (car p)))

(define second
  (lambda (p)
    (car (cdr p))))

(define build
  (lambda (s1 s2)
    (cons s1 (cons s2 '()))))
;; -----------------------------------------------------------

(define keep-looking
  (lambda (a sorn lat)
    (cond
      [(number? sorn) (keep-looking a (pick sorn lat) lat)]
      [else (eq? a sorn)])))


(define looking
  (lambda (a lat) (keep-looking a (pick 1 lat) lat)))

(check-true (looking 'caviar '(6 2 4 caviar 5 7 3)))
(check-false (looking 'caviar '(6 2 grits caviar 5 7 3)))


(define shift
  (lambda (pair)
    (build (first (first pair))
           (build (second (first pair) (second pair))))))

;; count the number of atom in a pair or item
(define length*
  (lambda (pora)
    (cond
      [(atom? pora) 1]
      [else (+ (length* (first pora)) (length* (second pora)))])))


(check-equal? (length* 'test) 1)
(check-equal? (length* '(hello there)) 2)

(define length
  (lambda (l)
    (cond
      [(null? l) 0]
      [else (add1 (length (cdr l)))])))


(define (eternity) "ERROR - should not get here")

;; suppose define no longer works
(lambda (l)
  (cond
    [(null? l) 0]
    [else (add1 (eternity (cdr l)))]))

;; length of empty list
(define length0
  (lambda (l)
    (if (null? l) 0
        (add1 (eternity (cdr l))))))
 
;; length of 1 item list
((lambda (l)
  (if (null? l) 0
      (add1 (length0 (cdr l)))))
 '(a))

;; but we are not allowed to use define so replace the definition of length0 with its body
((lambda (l)
   (if (null? l) 0
       (add1
        ((lambda (l)
          (if (null? l) 0
              (add1 (eternity (cdr l))))) (cdr l)))))
 '(a))

;; a function for a list of 2 items
((lambda (l)
   (if (null? l) 0
       (add1
        ((lambda (l)
          (if (null? l) 0
              (add1
               ((lambda (l)
                  (if (null? l) 0
                      (add1 (eternity (cdr l))))) (cdr l))))) (cdr l)))))
 '(a b))



;; we could write an infite function which would return the length for an infinite list
;; i.e the funciton 'length'

;; there is repetition of 'length' type behaviour which can be extracted. This will now
;; return a function which takes in the 'hukairs' functions and returns a function
;; to calculate length of 0 item list.
((lambda (length)
   (lambda (l)
     (if (null? l) 0
         (add1 (length (cdr l)))))) eternity)

;; rewrite length1 in the same style
((lambda (length)
  (lambda (l)
    (if (null? l) 0
        (add1 (length (cdr l))))))
 ((lambda (length)
    (lambda (l)
      (if (null? l) 0
          (add1 (length (cdr l)))))) eternity))

;; rewrite length2 in the same style
((lambda (length)
  (lambda (l)
    (if (null? l) 0
        (add1 (length (cdr l))))))
 ((lambda (length)
    (lambda (l)
      (if (null? l) 0
          (add1 (length (cdr l))))))
 ((lambda (length)
    (lambda (l)
      (if (null? l) 0
          (add1 (length (cdr l)))))) eternity)))

;; remove the repitition for the function which takes a 'length' and return a
;; function that looks like 'length'
((lambda (mk-length)
  (mk-length eternity))
(lambda (length)
  (lambda (l)
    (if (null? l) 0
        (add1 (length (cdr l)))))))

;; do the above for length1
((lambda (mk-length)
  (mk-length
   (mk-length eternity)))
(lambda (length)
  (lambda (l)
    (if (null? l) 0
        (add1 (length (cdr l)))))))

;; do the above for length2
((lambda (mk-length)
  (mk-length
   (mk-length
    (mk-length eternity))))
(lambda (length)
  (lambda (l)
    (if (null? l) 0
        (add1 (length (cdr l)))))))

;; do the above for length3
((lambda (mk-length)
  (mk-length
   (mk-length
    (mk-length
     (mk-length eternity)))))
(lambda (length)
  (lambda (l)
    (if (null? l) 0
        (add1 (length (cdr l)))))))

;; since nobody cares what function we pass to mk-length, we could pass mk-length
;; this is still length0

((lambda (mk-length)
  (mk-length mk-length))
(lambda (length)
  (lambda (l)
    (if (null? l) 0
        (add1 (length (cdr l)))))))

;; we could also use mk-length instead of length
((lambda (mk-length)
  (mk-length mk-length))
(lambda (mk-length)
  (lambda (l)
    (if (null? l) 0
        (add1 (mk-length (cdr l)))))))

;; now that mk-length is passed to mk-length, we can create an additional recrusive use
;; and redefine length1
(
 ((lambda (mk-length)
    (mk-length mk-length))
  (lambda (mk-length)
    (lambda (l)
      (if (null? l) 0
          (add1 ((mk-length eternity) (cdr l)))))))
 '(a))

;; if we pass mk-length to mk-length then we should be able to call it infinitely.
;; it works by passing mk-length to itself just as its about to expire
(
 ((lambda (mk-length)
    (mk-length mk-length))
  (lambda (mk-length)
    (lambda (l)
      (if (null? l) 0
          (add1 ((mk-length mk-length) (cdr l)))))))
 '(a))

;; extract the function which looks like length i.e (mk-length mk-length)
;; However this will go on forever as mk-length keeps getting applied to itself

;; THIS WILL NOT WORK
;((lambda (mk-length)
;    (mk-length mk-length))
;  (lambda (mk-length)
;    ((lambda (length)
;       (lambda (l)
;         (if (null? l) 0
;             (add1 (length (cdr l))))))
;     (mk-length mk-length))))


;; convert the (mk-length mk-length) invocation into a function

(
 ((lambda (mk-length)
    (mk-length mk-length))
  (lambda (mk-length)
    (lambda (l)
      (if (null? l) 0
          (add1
           ((lambda (x)
             ((mk-length mk-length) x)) (cdr l)))))))
 '(a))

;; move out the new function so that we get 'length' back

(
 ((lambda (mk-length)
    (mk-length mk-length))
  (lambda (mk-length)
    ((lambda (length)
       (lambda (l)
         (if (null? l) 0
             (add1 (length (cdr l))))))
     (lambda (x) ((mk-length mk-length) x)))))
 '(a))

;; now extract 'length' function so its separated

(
 ((lambda (le)
    ((lambda (mk-length)
       (mk-length mk-length))
     (lambda (mk-length)
       (le (lambda (x) ((mk-length mk-length) x))))))
  (lambda (length)
    (lambda (l)
      (if (null? l) 0
          (add1 (length (cdr l)))))))
 '(a))

;; generalize the function so that it can accept any 'length'-type functions

(lambda (le)
   ((lambda (mk-length)
      (mk-length mk-length))
    (lambda (mk-length)
      (le (lambda (x) ((mk-length mk-length) x))))))

;; we have arrived at the Y-Combinator

(define (Y g)
   ((lambda (f) (f f))
    (lambda (f) (g (lambda (x) ((f f) x))))))

;; use Y to simulate recursion without recursive calls

(define (length-Y arg)
  ((Y (lambda (mk-length)
       (lambda (l)
         (if (null? l) 0
             (add1 (mk-length (cdr l))))))) arg))

(check-equal? (length-Y '(a b c d e f)) 6)

(define (fact n)
  ((Y (lambda (mk-fact)
        (lambda (n)
          (if (= n 0) 1
              (* n (mk-fact (- n 1))))))) n))

(check-equal? (fact 5) 120)

;; verify Referential transparency by replacing fact with its definition

(check-equal? ((lambda (n)
                 ((Y (lambda (mk-fact)
                       (lambda (n)
                         (if (= n 0) 1
                             (* n (mk-fact (- n 1))))))) n)) 5)
              120)
