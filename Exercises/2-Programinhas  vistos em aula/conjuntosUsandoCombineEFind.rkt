#lang scheme
(define member? (lambda (elemento conjunto)
                 (find ((curry =) elemento)
                       conjunto)))
(define adiciona-lento (lambda (elemento conjunto)
                   (if (member? elemento conjunto)
                       conjunto
                       ((combine  cons id (list elemento))  conjunto))))

(define adiciona (lambda (elemento conjunto)
                   (if (member? elemento conjunto)
                       conjunto
                       (cons elemento  conjunto))))
(define uniao (lambda (conjunto1 conjunto2)
                   ((combine  cons id conjunto2)  conjunto1))))

(adiciona 5 '(4 3))
(cons (id 4)
      (cons (id 3)
            (list 5)))