#lang scheme
(define conj-vazio (lambda () '()))

(define membro? (lambda (elemento conjunto = )
   (if (null? conjunto) #f
     (if ((= elemento (car conjunto)) #t
         (membro? elemento (cdr conjunto) = )))))

(define adiciona (lambda (elemento conjunto = )
        (if (membro? elemento conjunto = ) conjunto
             (cons elemento conjunto))))