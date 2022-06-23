#lang Scheme

(define (insertion-sort comparador l )
  (if  (null? l) l 
       (insert comparador
               (car l) 
               (insertion-sort comparador (cdr l)))))
(define (insert comparador x l)
  (if (null? l) (list x)
      (if  (comparador x (car l)) (cons x l)
           (cons (car l) (insert comparador x (cdr l))))))
(insertion-sort < '(4 3 1 2))
