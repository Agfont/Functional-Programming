#lang scheme
(define combine (lambda (soma f zero) 
         (lambda (lista) 
              (if (null? lista) zero
                   (soma  (f (car lista)) 
                          ((combine soma f zero) (cdr lista)))))))
(define id (lambda (x) x))
(define find (lambda (pred lista)
       (if (null? lista) #f
            (if  (def(pred (car lista)) #t 
                 (find pred (cdr lista))))))
(define conj-vazio (lambda ()  '()))
(define adiciona-elem (lambda (elem conj)
       (if (member? elem conj) conj (cons elem conj))))
(define member? (lambda (elem conj)
          (find ((curry =) elem) conj)))
(define uniao (lambda (conj1 conj2)
          ((combine  adiciona-elem id conj2) conj1)))

