#lang scheme
(define (intervalo m n)
        (if (> m n) '()
             (cons m (intervalo (+ m 1) n))))

(define (remove-multiplos num lista)
      (if (null? lista) '()   
           (if (divide? num (car lista))
               (remove-multiplos num (cdr lista))
               (cons (car lista) (remove-multiplos num (cdr lista))))))

(define (divide? a b)
  (= (remainder b a) 0))

(define (filtro lista)
      (if (null? lista)
          lista
          (cons (car lista) 
                 (filtro (remove-multiplos (car lista) (cdr lista))))))
(define (primos<=n n)
               (filtro (intervalo  2 n)))