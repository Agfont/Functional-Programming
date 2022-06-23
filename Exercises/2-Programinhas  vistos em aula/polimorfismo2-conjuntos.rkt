#lang scheme
(define conj-vazio (lambda (=) (list = '())))
;num conjunto, (car conjunto) é a função de igualdade,
;num conjunto, (cadr conjunto) é uma lista com os elementos do conjunto
;ex: (list = '(1 2 3))
;com cons: (cons = (cons '(1 2 3) '()))
(define membro?-versao-longa (lambda (elemento conjunto)
   (if (null? (cadr conjunto)) #f
     (if ((car conjunto); função de igualdade é o primeiro elmemnto da lisa conjunto
          elemento
          (caadr conjunto)); compara com primeiro elemento da lista com o conjunto
         #t
         (membro? elemento (list (car conjunto) (cdadr conjunto)))))))
(define membro? (lambda (elemento conjunto)
   (find ((curry (car conjunto)) elemento)
         (cadr conjunto))))
                

(define adiciona (lambda (elemento conjunto)
        (if (membro? elemento conjunto)
            conjunto
            (list (car conjunto); mesma função de igualdade do conjunto original
                  (cons elemento (cadr conjunto)))))) ; acresenta elemento na lista de conteudo

;====funções auxiliares
(define find (lambda (predicado lista)
               (if (null? lista) #f
                   (if (predicado (car lista)) #t
                       (find predicado (cdr lista))))))

;===exemplos
(membro? 2 (list = '(1 2 3))); volta #t
(membro? 2 (adiciona 2 (conj-vazio =)));volta #t
(adiciona 2 (adiciona 3 (conj-vazio =))); volta conjunto com dois elementos
(adiciona 3 (adiciona 2 (adiciona 3 (conj-vazio =))))