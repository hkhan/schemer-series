#lang racket

(require rackunit)

(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

(define plus
  (lambda (a b)
    (cond
      [(zero? b) a]
      [else (add1 (plus a (sub1 b)))])))

(define equan?
  (lambda (a1 a2)
    (cond
      [(and (number? a1) (number? a2)) (= a1 a2)]
      [(or (number? a1) (number? a2)) #f]
      [else (eq? a1 a2)])))

(define rember*
  (lambda (a l)
    (cond
      [(null? l) '()]
      [(atom? (car l))
       (cond 
         [(eq? a (car l)) (rember* a (cdr l))]
         [else (cons (car l) (rember* a (cdr l)))])]
      [else (cons (rember* a (car l)) (rember* a (cdr l)))])))

(check-equal? (rember* 'cup '()) '())
(check-equal? (rember* 'cup '((cup))) '(()))
(check-equal? (rember* 'b '((b c d) b (b))) '((c d) ()))
(check-equal? (rember* 'cup '((coffee) cup ((tea) cup) (and (hick)) cup)) 
              '((coffee) ((tea)) (and (hick))))
(check-equal? (rember* 'sauce 
                       '(((tomato sauce)) ((bean) sauce) (and ((flying)) sauce)))
              '(((tomato)) ((bean)) (and ((flying)))))


(define insertR*
  (lambda (new old l)
    (cond
      [(null? l) '()]
      [(atom? (car l))
              (cond
                [(eq? old (car l)) (cons old (cons new (insertR* new old (cdr l))))]
                [else (cons (car l) (insertR* new old (cdr l)))])]
      [else (cons (insertR* new old (car l)) (insertR* new old (cdr l)))])))

(check-equal? (insertR* 'roast 'chuck
                        '((how much (wood)) 
                          could
                          ((a (wood) chuck))
                          (((chuck))) 
                          (if (a) ((wood chuck))) 
                          could chuck wood))
              '((how much (wood))
                could
                ((a (wood) chuck roast))
                (((chuck roast))) 
                (if (a) ((wood chuck roast))) 
                could chuck roast wood))


(define occur*
  (lambda (a l)
    (cond
      [(null? l) 0]
      [(atom? (car l))
       (cond
         [(eq? a (car l)) (add1 (occur* a (cdr l)))]
         [else (occur* a (cdr l))])]
      [else (plus (occur* a (car l)) (occur* a (cdr l)))])))

(check-equal? (occur* 'wood '((how much (wood)) 
                              could
                              ((a (wood) chuck))
                              (((chuck))) 
                              (if (a) ((wood chuck))) 
                              could chuck wood))
              4)
(check-equal? (occur* 'wood '()) 0)
(check-equal? (occur* 'wood '(test)) 0)


(define subst*
  (lambda (new old l)
    (cond
      [(null? l) '()]
      [(atom? (car l))
       (cond
         [(eq? old (car l)) (cons new (subst* new old (cdr l)))]
         [else (cons (car l) (subst* new old (cdr l)))])]
      [else (cons (subst* new old (car l)) (subst* new old (cdr l)))])))

(check-equal? (subst* 'hello 'world '()) '())
(check-equal? (subst* 'hello 'world '(test)) '(test))
(check-equal? (subst* 'stuff 'wood '((how much (wood))
                              could
                              ((a (wood) chuck))
                              (((chuck)))
                              (if (a) ((wood chuck)))
                              could chuck wood))
              '((how much (stuff))
                could
                ((a (stuff) chuck))
                (((chuck)))
                (if (a) ((stuff chuck)))
                could chuck stuff))

(define insertL*
  (lambda (new old l)
    (cond
      [(null? l) '()]
      [(atom? (car l))
       (cond
         [(eq? old (car l)) (cons new (cons old (insertL* new old (cdr l))))]
         [else (cons (car l) (insertL* new old (cdr l)))])]
      [else (cons (insertL* new old (car l)) (insertL* new old (cdr l)))])))

(check-equal? (insertL* 'pecker 'chuck '()) '())
(check-equal? (insertL* 'pecker 'chuck '(test)) '(test))
(check-equal? (insertL* 'pecker 'chuck '((how much (wood))
                              could
                              ((a (wood) chuck))
                              (((chuck)))
                              (if (a) ((wood chuck)))
                              could chuck wood))
              '((how much (wood))
                could
                ((a (wood) pecker chuck))
                (((pecker chuck)))
                (if (a) ((wood pecker chuck)))
                could pecker chuck wood))


(define member*
  (lambda (a l)
    (cond
      [(null? l) #f]
      [(atom? (car l)) (or (eq? a (car l)) (member* a (cdr l)))]
      [else (or (member* a (car l)) (member* a (cdr l)))])))

(check-false (member* 'pecker '()))
(check-false (member* 'pecker '(test)))
(check-true (member* 'chips '((potato) (chips ((with) fish) (chips)))))

(define leftmost
  (lambda (l)
    (cond
      [(atom? (car l)) (car l)]
      [else (leftmost (car l))])))

(check-equal? (leftmost '((potato) (chips ((with) fish) (chips)))) 'potato)
(check-equal? (leftmost '(((hot) (tuna (and))) cheese)) 'hot)


(define eqlist?
  (lambda (l1 l2)
    (cond
      [(and (null? l1) (null? l2)) #t]
      [(or (null? l1) (null? l2)) #f]
      [(and (atom? (car l1)) (atom? (car l2)))
            (and (equan? (car l1) (car l2)) (eqlist? (cdr l1) (cdr l2)))]
      [(or (atom? (car l1)) (atom? (car l2))) #f]
      [else (and (eqlist? (car l1) (car l2)) (eqlist? (cdr l1) (cdr l2)))])))

(check-true (eqlist? '() '()))
(check-true (eqlist? '(test) '(test)))
(check-true (eqlist? '(strawberry ice cream) '(strawberry ice cream)))
(check-false (eqlist? '(banana ((split))) '((banana) (split))))


(define equal?
  (lambda (s1 s2)
    (cond
      [(and (atom? s1) (atom? s2)) (equan? s1 s2)]
      [(or (atom? s1) (atom? s2)) #f]
      [else (eqlist? s1 s2)])))

(check-true (equal? 'test 'test))
(check-true (equal? '(test) '(test)))
(check-false (equal? 'test '(test)))
(check-false (equal? '() 'test))


(define eqlist?-rewrite
  (lambda (l1 l2)
    (cond
      [(and (null? l1) (null? l2)) #t]
      [(or (null? l1) (null? l2)) #f]
      [else (and (equal? (car l1) (car l2)) (eqlist?-rewrite (cdr l1) (cdr l2)))])))

(check-true (eqlist?-rewrite '() '()))
(check-true (eqlist?-rewrite '(test) '(test)))
(check-true (eqlist?-rewrite '(strawberry ice cream) '(strawberry ice cream)))
(check-false (eqlist?-rewrite '(banana ((split))) '((banana) (split))))

(define rember
  (lambda (s l)
    (cond
      [(null? l) '()]
      [(equal? s (car l)) (cdr l)]
      [else (cons (car l) (rember s (cdr l)))])))


(check-equal? (rember 'mint '(lamb chops and mint jelly))
              '(lamb chops and jelly))
(check-equal? (rember 'mint '(lamb and mint flavored mint jelly))
              '(lamb and flavored mint jelly))
(check-equal? (rember 'mint '(egg sandwich))
              '(egg sandwich))
(check-equal? (rember 'cup '(coffee cup tea cup and hick cup))
              '(coffee tea cup and hick cup))