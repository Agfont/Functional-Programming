#lang scheme
(define (atom? x)
        (if (null? x)
            #t
            (not (list? x)))) 
(define (equal? e1 e2) 
  (if (and (atom? e1) (atom? e2))
      (eq? e1 e2)
      (if (or  (atom? e1) (atom? e2))
          #f
          (and (equal? (car e1) (car e2))
               (equal? (cdr e1) (cdr e2))))))