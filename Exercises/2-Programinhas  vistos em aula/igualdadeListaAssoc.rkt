#lang scheme
(define sub-lista-assoc (lambda (lista-assoc1 lista-assoc2)
                          ;devo ser incapaz de encontrar uma chave em lista-assoc1
                          ;com valor DIFERENTE em lista-assoc2
                          ;para isso testo cada par chave-valor de lista-assoc1
                          ;contra lista-assoc2
                          (not (find (lambda (uma-assoc)
                                         (not (equal? (cadr uma-assoc)
                                                      (assoc (car uma-assoc) lista-assoc2))))
                                     lista-assoc1))))
(define equal-lista-assoc (lambda (lista-assoc1 lista-assoc2)
                            (and (sub-lista-assoc lista-assoc1 lista-assoc2)
                                 (sub-lista-assoc lista-assoc2 lista-assoc1))))