#lang racket

(require rackunit)

(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

;; definitions from previous exercises

(define member?
  (lambda (a lat)
    (cond
      [(null? lat) #f]
      [else (or (equal? a (car lat)) (member? a (cdr lat)))])))

(define multirember
  (lambda (a lat)
    (cond
      [(null? lat) '()]
      [(equal? a (car lat)) (multirember a (cdr lat))]
      [else (cons (car lat) (multirember a (cdr lat)))])))

(define firsts
  (lambda (l)
    (cond
      [(null? l) '()]
      [else (cons (car (car l)) (firsts (cdr l)))])))

;;--------------------------------------------------------------------------------

(define set?
  (lambda (lat)
    (cond
      [(null? lat) #t]
      [(member? (car lat) (cdr lat)) #f]
      [else (set? (cdr lat))])))

(check-true (set? '()))
(check-true (set? '(apples pears peaches plums)))
(check-false (set? '(fruits apple peaches apple plums)))
(check-false (set? '(apple 3 pear 4 9 apple 3 4)))

(define makeset
  (lambda (lat)
    (cond
      [(null? lat) '()]
      [(member? (car lat) (cdr lat)) (makeset (cdr lat))]
      [else (cons (car lat) (makeset (cdr lat)))])))

(check-equal? (makeset '()) '())
(check-equal? (makeset '(apple pear)) '(apple pear))
(check-equal? (makeset '(apple peach apple apple pear)) '(peach apple pear))


(define makeset-multirember
  (lambda (lat)
    (cond
      [(null? lat) '()]
      [else (cons (car lat) (makeset-multirember (multirember (car lat) (cdr lat))))])))

(check-equal? (makeset-multirember '()) '())
(check-equal? (makeset-multirember '(apple pear)) '(apple pear))
(check-equal? (makeset-multirember '(apple peach apple apple pear)) '(apple peach pear))
(check-equal? (makeset-multirember '(apple 3 pear 4 9 apple 3 4)) '(apple 3 pear 4 9))

(define subset?
  (lambda (set1 set2)
    (cond
      [(null? set1) #t]
      [else (and (member? (car set1) set2) (subset? (cdr set1) set2))])))

(check-true (subset? '() '(test)))
(check-true (subset? '() '()))
(check-true (subset? '(5 chicken wings) '(5 burgers
                                            2 pieces fried chicken and
                                            light duckling wings)))
(check-false (subset? '(chicken wings) '(light duckling wings)))

(define eqset?
  (lambda (set1 set2)
    (and (subset? set1 set2) (subset? set2 set1))))

(check-true (eqset? '() '()))
(check-false (eqset? '() '(chicken)))
(check-true (eqset? '(6 large chickens with wings) '(6 chickens with large wings)))
(check-false (eqset? '(6 large chickens with wings) '(6 chickens)))

(define intersect?
  (lambda (set1 set2)
    (cond
      [(null? set1) #f]
      [else (or (member? (car set1) set2) (intersect? (cdr set1) set2))])))

(check-false (intersect? '() '()))
(check-false (intersect? '() '(chicken)))
(check-true (intersect? '(6 large chickens with wings) '(6 macaroni cheese)))
(check-false (intersect? '(6 large chickens with wings) '(8 macaroni cheese)))


(define intersect
  (lambda (set1 set2)
    (cond
      [(null? set1) '()]
      [(member? (car set1) set2) (cons (car set1) (intersect (cdr set1) set2))]
      [else (intersect (cdr set1) set2)])))

(check-equal? (intersect '(stewed tomatoes and macaroni) 
                         '(macaroni and cheese)) '(and macaroni))
(check-equal? (intersect '(stewed tomatoes) 
                         '(macaroni and cheese)) '())


(define union
  (lambda (set1 set2)
    (cond
      [(null? set1) set2]
      [(member? (car set1) set2) (union (cdr set1) set2)]
      [else (cons (car set1) (union (cdr set1) set2))])))

(check-equal? (union '() '()) '())
(check-equal? (union '(stewed tomatoes and macaroni casserole)
                     '(macaroni and cheese))
              '(stewed tomatoes casserole macaroni and cheese))


(define difference
  (lambda (set1 set2)
    (cond
      [(null? set1) '()]
      [(member? (car set1) set2) (difference (cdr set1) set2)] 
      [else (cons (car set1) (difference (cdr set1) set2))])))

(check-equal? (difference '(stewed tomatoes) '(tomatoes)) '(stewed))


(define intersectall
  (lambda (l-set)
    (cond
      [(null? (cdr l-set)) (car l-set)]
      [else (intersect (car l-set) (intersectall (cdr l-set)))])))

(check-equal? (intersectall '((hello) (world))) '())
(check-equal? (intersectall '((6 pears and) (6 pears and) (6 pears))) '(6 pears))


(define a-pair?
  (lambda (x)
    (cond
      [(atom? x) #f]
      [(null? x) #f]
      [(null? (cdr x)) #f]
      [(null? (cdr (cdr x))) #t]
      [else #f])))

(check-false (a-pair? '()))
(check-false (a-pair? '(5)))
(check-false (a-pair? '(a b c)))
(check-false (a-pair? '((a b) c d)))
(check-false (a-pair? 'test))
(check-true (a-pair? '(3 7)))
(check-true (a-pair? '(2 (pair))))
(check-true (a-pair? '(full (house))))
(check-true (a-pair? '((3) (7))))


(define first
  (lambda (p)
    (car p)))

(check-equal? (first '(1 3)) 1)


(define second
  (lambda (p)
    (car (cdr p))))

(check-equal? (second '(1 3)) 3)


(define build
  (lambda (s1 s2)
    (cons s1 (cons s2 '()))))

(check-equal? (build 1 2) '(1 2))


(define third
  (lambda (l)
    (car (cdr (cdr l)))))

(check-equal? (third '(1 2 3)) 3)

(define fun?
  (lambda (rel)
    (set? (firsts rel))))

(check-true (fun? '((8 3) (4 2))))
(check-false (fun? '((d 4) (b 0) (b 9))))


(define revrel
  (lambda (rel)
    (cond
      [(null? rel) '()]
      [else (cons (build (second (car rel)) (first (car rel)))
                  (revrel (cdr rel)))])))

(check-equal? (revrel '((8 a) (pumpkin pie))) '((a 8) (pie pumpkin)))

(define revpair
  (lambda (pair)
    (build (second pair) (first pair))))


(define revrel2
  (lambda (rel)
    (cond
      [(null? rel) '()]
      [else (cons (revpair (car rel)) (revrel2 (cdr rel)))])))

(check-equal? (revrel2 '((8 a) (pumpkin pie))) '((a 8) (pie pumpkin)))


(define seconds
  (lambda (l)
    (cond
      [(null? l) '()]
      [else (cons (second (car l)) (seconds (cdr l)))])))

(check-equal? (seconds '((1 2) (3 4))) '(2 4))

(define fullfun?
  (lambda (fun)
    (set? (seconds fun))))

(check-true (fullfun? '((8 3) (4 2) (5 9))))
(check-false (fullfun? '((8 3) (4 2) (5 2))))
(check-false (fullfun? '((grape raisin) (plum prune) (stewed prune))))
(check-true (fullfun? '((grape raisin) (plum prune) (stewed grape))))


(define one-to-one?
  (lambda (fun)
    (fun? (revrel2 fun))))

(check-true (one-to-one? '((8 3) (4 2) (5 9))))
(check-false (one-to-one? '((8 3) (4 2) (5 2))))
(check-false (one-to-one? '((grape raisin) (plum prune) (stewed prune))))
(check-true (one-to-one? '((grape raisin) (plum prune) (stewed grape))))
