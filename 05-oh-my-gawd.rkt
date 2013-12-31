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
