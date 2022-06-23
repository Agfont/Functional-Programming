#lang scheme
(define find (lambda (pred lista)
               (and (not (null? lista)) 
                    (or (pred (car lista))
                       (find pred (cdr lista))))))