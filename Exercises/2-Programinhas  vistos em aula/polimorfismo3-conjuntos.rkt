#lang scheme
(define gera-operacoes-conjuntos
  (lambda (=)
    (list (lambda () '());==>conjunto vazio
          (lambda (elemento conjunto)
            (find ((curry =) elemento)
                  conjunto));====> membro?
          (lambda (elemento conjunto)
            (if (find ((curry =) elemento)
                      conjunto)
            conjunto
            (cons elemento conjunto)));====>adiciona
          )
    ))


;====funções auxiliares
(define find (lambda (predicado lista)
               (if (null? lista) #f
                   (if (predicado (car lista)) #t
                       (find predicado (cdr lista))))))


;===exemplos
(define ops (gera-operacoes-conjuntos =))
(define vazio (car ops))
(define membro? (cadr ops))
(define adiciona (caddr ops))
(vazio)
(adiciona 5 (vazio))
(membro? 5 (adiciona 5 (adiciona 4 (vazio))))
(membro? 7 (adiciona 5 (adiciona 4 (vazio))))