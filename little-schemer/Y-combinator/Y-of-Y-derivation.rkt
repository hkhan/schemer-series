#lang racket

(require rackunit)

(define (hukairs)
  ("ERROR - should not get here"))

(define (length l)
  (if (null? l) 0
      (add1 (length (cdr l)))))

(check-equal? (length '(1 2 3)) 3)

; step 1: remove the recursive call to length
; now the function will work for length 0 but fail for everything else

(define (length0 l)
  (if (null? l) 0
      (add1 (hukairs (cdr l)))))

(check-equal? (length0 '()) 0)
;(check-equal? (length0 '(test)) 1) FAIL

;; step 2: now make it work for list of length 1

(define (length1 l)
  (if (null? l) 0
      (add1 (length0 (cdr l)))))

(check-equal? (length1 '()) 0)
(check-equal? (length1 '(test)) 1)
; (check-equal? (length1 '(x y)) 2) FAIL

;; step 2: now make it work for list of length 1

(define (length2 l)
  (if (null? l) 0
      (add1 (length1 (cdr l)))))

(check-equal? (length2 '()) 0)
(check-equal? (length2 '(test)) 1)
(check-equal? (length2 '(x y)) 2)
; (check-equal? (length1 '(x y z)) 3) FAIL

;; step 3: rewrite length0 so that length reappears and we pass in the
;; seed function i.e curry the function

(
 ((lambda (length)
    (lambda (l)
      (if (null? l) 0
          (add1 (length (cdr l)))))) hukairs)
 '())

;; step 4: rewrite length1 in the same style as above

(
 ((lambda (length)
    (lambda (l)
      (if (null? l) 0
          (add1 (length (cdr l))))))
  ((lambda (length)
     (lambda (l)
       (if (null? l) 0
           (add1 (length (cdr l)))))) hukairs))
 '(1))

;; step 5: rewrite length2 in the same style as above

(
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
            (add1 (length (cdr l))))))
    hukairs)))
 '(1 2))

;; step 6: extract the function which looks like length for length0
(
 ((lambda (mk-length)
    (mk-length hukairs))
  (lambda (length)
    (lambda (l)
      (if (null? l) 0
          (add1 (length (cdr l))))))) 
 '())

;; step 7: repeat for length1
(
 ((lambda (mk-length)
    (mk-length (mk-length hukairs)))
  (lambda (length)
    (lambda (l)
      (if (null? l) 0
          (add1 (length (cdr l))))))) 
 '(a))

;; step 8: repeat for length2
(
 ((lambda (mk-length)
    (mk-length (mk-length (mk-length hukairs))))
  (lambda (length)
    (lambda (l)
      (if (null? l) 0
          (add1 (length (cdr l))))))) 
 '(a b))

;; step 9: repeat for length3
(
 ((lambda (mk-length)
    (mk-length (mk-length (mk-length (mk-length hukairs)))))
  (lambda (length)
    (lambda (l)
      (if (null? l) 0
          (add1 (length (cdr l))))))) 
 '(a b c))

;; step 10: since we don't care about the hukairs we could pass mk-length
(
 ((lambda (mk-length)
    (mk-length mk-length))
  (lambda (mk-length)
    (lambda (l)
      (if (null? l) 0
          (add1 (mk-length (cdr l))))))) 
 '())

;; step 11: repeat for length1
(
 ((lambda (mk-length)
    (mk-length mk-length))
  (lambda (mk-length)
    (lambda (l)
      (if (null? l) 0
          (add1 ((mk-length hukairs) (cdr l))))))) 
 '(a))

;; step 12: repeat for length3
(
 ((lambda (mk-length)
    (mk-length mk-length))
  (lambda (mk-length)
    (lambda (l)
      (if (null? l) 0
          (add1 ((mk-length mk-length) (cdr l))))))) 
 '(a b c))

;; step 14: extract the self application and call that length
(
 ((lambda (mk-length)
    (mk-length mk-length))
  (lambda (mk-length)
    ((lambda (length)
      (lambda (l)
        (if (null? l) 0
            (add1 (length (cdr l))))))
     (lambda (x) ((mk-length mk-length) x)))))
 '(a b c))

;; step 15: extract the function which looks like length
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
 '(a b c))

;; step 16: separate the function that makes length from the function
;; that looks like length

(lambda (le)
    ((lambda (mk-length)
       (mk-length mk-length))
     (lambda (mk-length)
       (le (lambda (x) ((mk-length mk-length) x))))))

;; step 17: We have arrived at the Y-Combinator

(define (Y le)
    ((lambda (f) (f f))
     (lambda (f) (le (lambda (x) ((f f) x))))))

(define (f mk-length)
  (lambda (l)
    (if (null? l) 0
        (add1 (mk-length (cdr l))))))
     
(define length-Y (Y f))
(length-Y '(a b c d e))

(define (fact n)
  ((Y (lambda (mk-fact)
        (lambda (n)
          (if (= n 0) 1
              (* n (mk-fact (- n 1))))))) n))
(fact 5)


  
