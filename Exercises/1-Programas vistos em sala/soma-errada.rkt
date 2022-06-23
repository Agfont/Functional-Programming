#lang scheme
;esta função não funciona
;não temos variáveis locais nesta versão da linguagem
;e o uso de variáveis globais não dá o resultado certo em recursão
(define tmp 0)
(define (soma-errada  l ) 
  (if (null? l) tmp
      (if (number? l) (+ tmp l)
          (begin (set! tmp (soma-errada (car l)))
                 (+ tmp (soma-errada (cdr l)))))))