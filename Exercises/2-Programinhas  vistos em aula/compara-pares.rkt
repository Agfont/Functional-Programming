#lang Scheme
(define compara-pares
  (lambda (par1 par2)
    (if (< (car par1) (car par2))
        #t
        (if (< (car par2) (car par1))
            #f
            (< (cadr par1) (cadr par2))))))
