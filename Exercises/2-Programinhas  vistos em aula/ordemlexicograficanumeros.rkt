#lang scheme
(define ordem-lexicografica-numeros
  (lambda (lista1 lista2)
    (if (null? lista1)
        #t
        (if (null? lista2)
            #f
            (if (< (car lista1) (car lista2))
                #t
                (if (> (car lista1) (car lista2))
                    #f
                    (ordem-lexicografica-numeros (cdr lista1) (cdr lista2))))))))
(ordem-lexicografica-numeros '(4 5) '(4 3))