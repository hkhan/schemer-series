#lang racket

(require rackunit)

(define (debug text number)
  (string-append text (number->string number)))

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

; step 3: use a let expression to eliminate duplication

(let
    ([f (lambda (f n)
          (if (= n 0) 1
              (* n (f f (- n 1)))))])
  (f f 5))

; step 4: LET to LAMBDA transformation, (let ([x val]) body) => ((lambda (x) body) val)

((lambda (f) (f f 5))
 (lambda (f n)
          (if (= n 0) 1
              (* n (f f (- n 1))))))
 
; step 5: at present, this only works for constant 5 so make that in to a variable

((lambda (f x) (f f x))
 (lambda (f n)
          (if (= n 0) 1
              (* n (f f (- n 1))))) 5)


; step 6: curry the function so that it takes 2 args, one at a time

(debug "step 6: "
(((lambda (f) (lambda (x) ((f f) x)))
 (lambda (f)
   (lambda (n)
     (if (= n 0) 1
         (* n ((f f) (- n 1))))))) 5)
 )

;; step 7: we can name this curried function to be fact so that we can call it as normal
(debug "step 7: "
       
(((lambda (f) (lambda (x) ((f f) x)))
 (lambda (f)
   (lambda (n)
     (if (= n 0) 1
         (* n ((f f) (- n 1)))))))
 5)
)

;; step 8: the above looks fine but has a minor issue where f has to apply itself for every
;; recursion. we can give this self-application a name

(debug "step 8: "
       
(((lambda (f) (lambda (x) ((f f) x)))
 (lambda (f)
   (let ([s (lambda (x) ((f f) x))])
     (lambda (n)
       (if (= n 0) 1
           (* n (s (- n 1))))))))
 5)
)

; step 9: LET to LAMBDA transformation, (let ([x val]) body) => ((lambda (x) body) val.
; Now this looks like any ordinary function application

(debug "step 9: "
       
(((lambda (f) (lambda (x) ((f f) x)))
 (lambda (f)
   ((lambda (s)
     (lambda (n)
       (if (= n 0) 1
           (* n (s (- n 1))))))
      (lambda (x) ((f f) x)))))
 5)
)


; step 10: give the factorial function a name, g

(debug "step 10: "
       
(((lambda (f) (lambda (x) ((f f) x)))
 (lambda (f)
   (let ([g (lambda (s)
              (lambda (n)
                (if (= n 0) 1
                    (* n (s (- n 1))))))])
      (g (lambda (x) ((f f) x))))))
 5)
)

; step 11: LET to LAMBDA transformation, (let ([x val]) body) => ((lambda (x) body) val).

(debug "step 11: "
       
(
 ((lambda (f) (lambda (x) ((f f) x)))
  (lambda (f) ((lambda (g)
                 (g (lambda (x) ((f f) x))))
    (lambda (s)
              (lambda (n)
                (if (= n 0) 1
                    (* n (s (- n 1)))))))))
 5)
)

;; step 12: Isolate the combinator using eta expansion

(debug "step 12: "
       
(
 ((lambda (r)
   ((lambda (f) (lambda (x) ((f f) x)))
    (lambda (f) ((lambda (g)
                   (g (lambda (x) ((f f) x))))
                 r))))
  (lambda (s)
    (lambda (n)
      (if (= n 0) 1
          (* n (s (- n 1)))))))
 5)
)

;; step 13 : apply beta reduction for r

(debug "step 13: "
       
(
 ((lambda (r)
   ((lambda (f) (lambda (x) ((f f) x)))
    (lambda (f) (r (lambda (x) ((f f) x))))))
  (lambda (s)
    (lambda (n)
      (if (= n 0) 1
          (* n (s (- n 1)))))))
 5)
)

;; step 14 : eta reduction of first (lambda (x) ((f f) x))

(debug "step 14: "
       
(
 ((lambda (r)
   ((lambda (f) (f f))
    (lambda (f) (r (lambda (x) ((f f) x))))))
  (lambda (s)
    (lambda (n)
      (if (= n 0) 1
          (* n (s (- n 1)))))))
 5)
)

; step 15: name the combinator

(define (Y r)
  ((lambda (f) (f f))
   (lambda (f) (r (lambda (x) ((f f) x))))))

(define (f s)
  (lambda (n)
    (if (= n 0) 1
        (* n (s (- n 1))))))

(debug "step 15: " ((Y f) 5))
(debug "step 15a: " ((f (Y f)) 5))



