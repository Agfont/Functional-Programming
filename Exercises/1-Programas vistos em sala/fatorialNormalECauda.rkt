#lang scheme
(define (fat n)
  (if (= n 1) 1
      (* n (fat (- n 1)))))
(define (fat-cauda n) (fat-aux n 1))
(define (fat-aux n tmp)
  (if (=  n 1) tmp
      (fat-aux (- n 1) (* n tmp))))
