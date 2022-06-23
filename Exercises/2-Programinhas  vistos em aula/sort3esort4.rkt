#lang scheme
(define compara-pares (lambda (par1 par2)
      (if (< (car par1) (car par2)) 
           #t
          (if (< (car par2) (car par1))
               #f
               (< (cadr par1) (cadr par2))))))
(define compara-pares-de-pares (lambda (t1 t2)
                                 (if (compara-pares (car t1) (car t2)) #t
                                 (if (compara-pares (car t2) (car t1)) #f
                                                    (compara-pares (cadr t1) (cadr t2))))))
(define ordem-lexicografica-pares  (lambda (<1 <2)
     (lambda (p1 p2)
        (if (<1 (car p1) (car p2)) #t
            (if (<1 (car p2) (car p1)) #f
                 (<2 (cadr p1) (cadr p2)))))))
(define primeiro-menor-segundo-maior (ordem-lexicografica-pares < >))
(define seleciona2posicoes (lambda (numero-posicao1 numero-posicao2)                            
        (lambda (lista)
               (list (enesimo numero-posicao1 lista) (enesimo numero-posicao2 lista)))))
(define enesimo (lambda (n lista)
        (if (= n 0) (car lista)
            (enesimo (- n 1)  (cdr lista)))))
(define compoe2-1 (lambda (f-bin f-un)
                    (lambda (x y) (f-bin (f-un x) (f-un y)))))
(define compara-colunas (lambda (num-col1 num-col2)
                          (compoe2-1 (ordem-lexicografica-pares < <)
                                     (seleciona2posicoes num-col1 num-col2))))