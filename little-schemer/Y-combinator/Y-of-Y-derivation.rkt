#lang racket

(require rackunit)

;; Y combinator derivation in detailed steps 
;; (based on Why of Y derivation by Matthias Felleisen)  

;; Is it possible to define a recursive function without referrring to itself?
;; Sounds almost magical but lets give it a go.

;; Define a recursive simple recursive function which refers to itself.
;; The aim is to remove the call to length from within itself.
(define (length l)
  (if (null? l) 0
      (add1 (length (cdr l)))))

(check-equal? (length '(1 2 3)) 3)

;; Step 1: remove the recursive call to length.
;; Now it will work for length 0 but fail for everything else but its a start.
(define (length0 l)
  (if (null? l) 0
      (add1 (hukairs (cdr l)))))

;; aptly named as this function should never be called
(define (hukairs)
  ("ERROR - should not get here"))

(check-equal? (length0 '()) 0)
; (check-equal? (length0 '(x)) 2) FAIL

;; Step 2: now make it work for a list of length 1 by calling length0
(define (length1 l)
  (if (null? l) 0
      (add1 (length0 (cdr l)))))

(check-equal? (length1 '()) 0)
(check-equal? (length1 '(test)) 1)

;; Step 3: now make it work for list of length 2 by calling length1...I see a pattern
(define (length2 l)
  (if (null? l) 0
      (add1 (length1 (cdr l)))))

(check-equal? (length2 '()) 0)
(check-equal? (length2 '(test)) 1)
(check-equal? (length2 '(x y)) 2)
; (check-equal? (length1 '(x y z)) 3) FAIL

;; From here on in, we can just use anonymous functions instead of redefining functions
;; Step 4: rewrite length0 so that length reappears and we pass in the seed function
;; (which ofcourse shouldn't be called for list of length 0.
(
 ((lambda (length)
    (lambda (l)
      (if (null? l) 0
          (add1 (length (cdr l)))))) hukairs)
 '())
;; -> 0

;; Step 5: rewrite length1 in the same style as above where we immediately call the
;; new function passing in the length0 definition from Step 4
(
 ((lambda (length)
    (lambda (l)
      (if (null? l) 0
          (add1 (length (cdr l))))))
  ((lambda (length)                                ; 'inlined' definition from step 4
     (lambda (l)
       (if (null? l) 0
           (add1 (length (cdr l)))))) hukairs))
 '(1))
;; -> 1

;; Step 6: rewrite length2 in the same style as above...I see a pattern here.
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
            (add1 (length (cdr l)))))) hukairs)))
 '(1 2))
;; -> 2

;; Step 7: back to length0 - extract the function which looks like length for length0.
;; Now we call the passed in function with hukairs which should never be called.
(
 ((lambda (mk-length)
    (mk-length hukairs))
  (lambda (length)
    (lambda (l)
      (if (null? l) 0
          (add1 (length (cdr l))))))) 
 '())
;; -> 0

;; Step 8: repeat the above step for length1 i.e. make another call to the
;; passed in length-like function
(
 ((lambda (mk-length)
    (mk-length (mk-length hukairs)))
  (lambda (length)
    (lambda (l)
      (if (null? l) 0
          (add1 (length (cdr l))))))) 
 '(a))
;; -> 1

;; Step 9: repeat for length2....I can see the pattern again.
(
 ((lambda (mk-length)
    (mk-length (mk-length (mk-length hukairs))))
  (lambda (length)
    (lambda (l)
      (if (null? l) 0
          (add1 (length (cdr l))))))) 
 '(a b))
;; -> 2

;; Step 10: repeat for length3...just to make sure.
(
 ((lambda (mk-length)
    (mk-length (mk-length (mk-length (mk-length hukairs)))))
  (lambda (length)
    (lambda (l)
      (if (null? l) 0
          (add1 (length (cdr l))))))) 
 '(a b c))
;; -> 3

;; Step 11: back to length0 variant - since we don't care about the hukairs,
;; we could pass mk-length to mk-length instead of hukairs and no one would care.
(
 ((lambda (mk-length)
    (mk-length mk-length))
  (lambda (mk-length)
    (lambda (l)
      (if (null? l) 0
          (add1 (mk-length (cdr l))))))) 
 '())
;; -> 0

;; Step 12: in order to create length1, we can now call the passed in function with
;; hukairs function as that will never be called
(
 ((lambda (mk-length)
    (mk-length mk-length))
  (lambda (mk-length)
    (lambda (l)
      (if (null? l) 0
          (add1 ((mk-length hukairs) (cdr l))))))) 
 '(a))
;; -> 1

;; Step 13: in order to create length2, we can replace hukairs with the passed in
;; mk-length which means that as the recursion process is about to terminate and it
;; requires one more call, it won't call mk-length with hukaris (which was the case
;; in previous steps). Instead it will call the passed-in function (mk-length) with
;; the same passed in function (mk-length) which can reach the termination condition
;; (null? l) or call itself again, if required....MAGIC
(
 ((lambda (mk-length)
    (mk-length mk-length))
  (lambda (mk-length)
    (lambda (l)
      (if (null? l) 0
          (add1 ((mk-length mk-length) (cdr l))))))) 
 '(a b))
;; -> 2

;; Now its just a matter of refactoring and generalizing
;; Step 14: extract the self application and call that length.
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
;; -> 3

;; Step 15: extract the function which looks like length
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
;; -> 3

;; Step 16: separate the function that makes length from the function
;; that looks like length (le). Notice that mk-length is bound separately in two
;; different scopes
(lambda (le)
  ((lambda (mk-length)                               
     (mk-length mk-length))
   (lambda (mk-length)
     (le (lambda (x) ((mk-length mk-length) x))))))
;; -> <procedure>

;; Step 17: give the above a name Y and we have arrived at the Y-Combinator
(define (Y le)
  ((lambda (f) (f f))
   (lambda (g) (le (lambda (x) ((g g) x))))))

;; back to where we started from....define a function which look like length....
(define (f mk-length)
  (lambda (l)
    (if (null? l) 0
        (add1 (mk-length (cdr l))))))

;; and pass that to Y to return a function which is length (without referring to itself)
(define length-Y (Y f))

(check-equal? (length-Y '(a b c d e)) 5)

;; also works for factorials and all other functions which can be defined recursively.
;; (fact is not referenced from within the body)
(define (fact n)
  ((Y (lambda (mk-fact)
        (lambda (n)
          (if (= n 0) 1
              (* n (mk-fact (- n 1))))))) n))

(check-equal? (fact 5) 120)

;; MAGICAL....did I hear Lambda Calculus......