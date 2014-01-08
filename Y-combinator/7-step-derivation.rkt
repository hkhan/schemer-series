#lang racket

(require rackunit)

; step 1
((lambda (n f)
   (if (= n 0) 1
       (* n (f (- n 1) f))))
 5
 (lambda (n f)
   (if (= n 0) 1
       (* n (f (- n 1) f)))))


; step 2: re-order the arguments so that f is followed by n

((lambda (f n)
   (if (= n 0) 1
       (* n (f f (- n 1)))))
 (lambda (f n)
   (if (= n 0) 1
       (* n (f f (- n 1)))))
 5)

; step 2a: use a let expression to eliminate duplication

(let
    ((f (lambda (f n)
          (if (= n 0) 1
              (* n (f f (- n 1)))))))
  (f f 5))

; step 3: curry the function so that it takes 2 args, one at a time

(((lambda (f)
    (lambda (n)
      (if (= n 0) 1
          (* n ((f f) (- n 1))))))
 (lambda (f)
   (lambda (n)
     (if (= n 0) 1
       (* n ((f f) (- n 1)))))))
 5)

;; step 4: we can name this curried function to be fact so that we can call it as normal

(define fact
  ((lambda (f)
    (lambda (n)
      (if (= n 0) 1
          (* n ((f f) (- n 1))))))
 (lambda (f)
   (lambda (n)
     (if (= n 0) 1
         (* n ((f f) (- n 1))))))))
  
(check-equal? (fact 5) 120)

;; step 5:
