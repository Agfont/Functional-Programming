(test
  (interpS '(let ([Wallet
                    (class Object money
                           (method credit amount (:= money (+ money amount)))
                           (method debit amount (:= money (- money amount))) )])
              (let ([WalletWithTaxes
                      (class Wallet tax
                             (method credit amount (:= money (- (+ money amount) tax)))
                             (method total dummy money) )])
                (let ([wallet (new WalletWithTaxes 1)])
                  (seq (send wallet credit 10)
                       (seq (send wallet debit 3)
                            (send wallet total 0))
                       )))))
  (numV 6))
