#lang plai-typed
; este interpretador aumenta o closureTyped para incluir
; cons, car, cdr, valor nulo (descrito como  ())
; e display
; display imprime o valor passado seguido de um ";". Nao mudamos de linha 

; Basic expressions
(define-type ExprC
  [numC  (n : number)]
  [idC   (s : symbol)]
  [plusC (l : ExprC) (r : ExprC)]
  [multC (l : ExprC) (r : ExprC)]
  [lamC  (arg : symbol) (body : ExprC)]
  [appC  (fun : ExprC) (arg : ExprC)]
  [ifC   (c : ExprC) (y : ExprC) (n : ExprC)]
  [seqC  (e1 : ExprC) (e2 : ExprC)]
  [setC  (var : symbol) (arg : ExprC)]
  [letC  (name : symbol) (arg : ExprC) (body : ExprC)]
  [consC (car : ExprC) (cdr : ExprC)]
  [carC  (cell : ExprC) ]
  [cdrC (cell : ExprC)]
  [displayC (exp : ExprC)]
  [quoteC  (sym : symbol)]
  [nullC  ]
  [equalC  (x : ExprC) (y : ExprC)]
  [let*C (n1 : symbol) (a1 : ExprC) (n2 : symbol) (a2 : ExprC) (body : ExprC)]
  [letrecC (s : symbol) (f : ExprC) (body : ExprC)]
  )


; Sugared expressions
(define-type ExprS
  [numS    (n : number)]
  [idS     (s : symbol)]
  [lamS    (arg : symbol) (body : ExprS)]
  [appS    (fun : ExprS) (arg : ExprS)]
  [plusS   (l : ExprS) (r : ExprS)]
  [bminusS (l : ExprS) (r : ExprS)]
  [uminusS (e : ExprS)]
  [multS   (l : ExprS) (r : ExprS)]
  [ifS     (c : ExprS) (y : ExprS) (n : ExprS)]
  [seqS    (e1 : ExprS) (e2 : ExprS)]
  [setS    (var : symbol) (arg : ExprS)]
  [letS    (name : symbol) (arg : ExprS) (body : ExprS)]
  [consS (car : ExprS) (cdr : ExprS)]
  [carS (cell : ExprS) ]
  [cdrS (cell : ExprS)]
  [displayS (exp : ExprS)]
  [quoteS  (sym : symbol)]
  [nullS ]
  [equalS  (x : ExprS) (y : ExprS)]
  [let*S   (name1 : symbol) (arg1 : ExprS) (name2 : symbol) (arg2 : ExprS) (body : ExprS)]
  [letrecS (s : symbol) (f : ExprS) (body : ExprS)]
 )


; Removing the sugar
(define (desugar [as : ExprS]) : ExprC
  (type-case ExprS as
    [numS    (n)        (numC n)]
    [idS     (s)        (idC s)]
    [lamS    (a b)      (lamC a (desugar b))]
    [appS    (fun arg)  (appC (desugar fun) (desugar arg))]
    [plusS   (l r)      (plusC (desugar l) (desugar r))]
    [multS   (l r)      (multC (desugar l) (desugar r))]
    [bminusS (l r)      (plusC (desugar l) (multC (numC -1) (desugar r)))]
    [uminusS (e)        (multC (numC -1) (desugar e))]
    [ifS     (c s n)    (ifC (desugar c) (desugar s) (desugar n))]
    [seqS    (e1 e2)    (seqC (desugar e1) (desugar e2))]
    [setS    (var expr) (setC  var (desugar expr))]
    [letS    (n a b)    (letC n (desugar a) (desugar b))]
    [consS   (car cdr)  (consC (desugar car) (desugar cdr))]
    [carS    (exp)      (carC (desugar  exp)) ]
    [cdrS    (exp)      (cdrC (desugar  exp)) ]
    [displayS (exp)     (displayC (desugar exp))]
    [quoteS (sym)       (quoteC sym)]
    [nullS  ()          (nullC)]
    [equalS  (x y)      (equalC (desugar x) (desugar y))]
    [let*S   (n1 a1 n2 a2 body) (let*C n1 (desugar a1) n2 (desugar a2) (desugar body))]
    [letrecS (s f body) (letrecC s (desugar f) (desugar body))]
    ))


; We need a new value for the box
(define-type Value
  [numV  (n : number)]
  [nullV ]
  [quoteV (symb : symbol)]
  [closV (arg : symbol) (body : ExprC) (env : Env)]
  [cellV (first : Value) (second : Value)]
  [boolV (b : boolean)]
  [suspV (exp : ExprC) (env : Env)]
  [locationV (num : number)]
  )

; Location
(define-type-alias Location number)
; Bindings associate symbol with location
(define-type Binding
        [bind (name : symbol) (val : Location)])

; Env remains the same, we only change the Binding
(define-type-alias Env (listof Binding))
(define mt-env empty)
(define extend-env cons)

; Storage
(define-type Storage
        [cell (location : Location) (val : (boxof Value))])
(define-type-alias Store (listof Storage))
(define mt-store empty)
(define override-store cons)

(define-type Result
        [v*s (v : Value) (s : Store)])

(define new-loc
  (let ([n (box 0)])
        (lambda () : Location
          (begin
            (set-box! n (+ 1 (unbox n)))
            (unbox n)))))

; Find the name of a variable
(define (lookup [for : symbol] [env : Env]) : Location
       (cond
            [(empty? env) (error 'lookup (string-append (symbol->string for) " was not found"))] ; variable is undefined
            [else (cond
                  [(symbol=? for (bind-name (first env)))   ; found it!
                                 (bind-val (first env))]
                  [else (lookup for (rest env))])]))        ; check in the rest

(define (fetch [loc : Location] [sto : Store]) : (boxof Value)
       (cond
            [(empty? sto) (error 'fetch (string-append (to-string loc) " was not found"))] ; variable is undefined
            [else (cond
                  [(= loc (cell-location (first sto)))   ; found it!
                                 (cell-val (first sto))]
                  [else (fetch loc (rest sto))])]))        ; check in the rest

; Auxiliary operators
(define (num+ [l : Value] [r : Value]) : Value
    (cond
        [(and (numV? l) (numV? r))
             (numV (+ (numV-n l) (numV-n r)))]
        [else
             (error 'num+ "One of the arguments is not a number")]))

(define (num* [l : Value] [r : Value]) : Value
    (cond
        [(and (numV? l) (numV? r))
             (numV (* (numV-n l) (numV-n r)))]
        [else
             (error 'num* "One of the arguments is not a number")]))

; Interpreter
(define (interp [a : ExprC] [env : Env] [sto : Store]) : Result
  (type-case ExprC a
    ; Numbers just evaluta to their equivalent Value
    [numC (n) (v*s (numV n) sto)]

    ; IDs are retrieved from the Env and unboxed
    [idC (n) (let* ([location (lookup n env)]
                    [val (fetch location sto)])
               (type-case Value (unbox val)
                 [suspV (exp env)
                        (let ([result (interp (suspV-exp (unbox val)) (suspV-env (unbox val)) sto)])
                        (begin
                             (set-box! val (v*s-v result))
                              result))]
                 [else (v*s (unbox val) sto)]
                 ))]

    ; Lambdas evaluate to closures, which save the environment
    [lamC (a b) (v*s (closV a b env) sto)]

    ; Application of function
    [appC (f a)
          (type-case Result (interp f env sto)
              [v*s (v-f s-f)
                   (type-case Result (interp a env s-f)
                     [v*s (v-a s-a)
                          (let ([onde (new-loc)])
                            (interp (closV-body v-f)
                                    (extend-env (bind (closV-arg v-f) onde)
                                                (closV-env v-f))
                                    (override-store (cell onde (box (suspV a env))) s-a)))
                          ])])]

    ;Sum two numbers using auxiliary function
    [plusC (l r) (type-case Result (interp l env sto)
               [v*s (v-l s-l)
                    (type-case Result (interp r env s-l)
                      [v*s (v-r s-r)
                           (v*s (num+ v-l v-r) s-r)])])]

    ; Multiplies two numbers using auxiliary function
    [multC (l r) (type-case Result (interp l env sto)
               [v*s (v-l s-l)
                    (type-case Result (interp r env s-l)
                      [v*s (v-r s-r)
                           (v*s (num* v-l v-r) s-r)])])]

    ; Conditional operator
    [ifC (c s n) (let ([val (interp c env sto)])
                    (if (zero? (numV-n (v*s-v val)))
                     (interp n env (v*s-s val))
                     (interp s env (v*s-s val))))]
    
    ; Sequence of operations
    [seqC (b1 b2)
        (type-case Result (interp b1 env sto)
                  [v*s (v-b1 s-b1)
                        (interp b2 env s-b1)])] ; No side effect between expressions!

    ; Attribution of variables
    [setC (var val) (let ([b (fetch (lookup var env) sto)])
                       (begin (set-box! b (v*s-v (interp val env sto))) (v*s (unbox b) sto)))]

    ; Declaration of variable
    [letC (name arg body)
          (let* ([where (new-loc)]
                 [new-bind (bind name where)]
                 [new-cell (cell where (box (suspV arg env)))]
                 [new-env (extend-env new-bind env)]
                 [new-sto (override-store new-cell sto)]
                 [b (fetch (lookup name new-env) new-sto)])
            (interp body new-env new-sto))]
    
    ; Cell operations
    [consC (car cdr)
           (let* ([where1 (new-loc)]
                 [where2 (new-loc)]
                 [new-store1 (override-store (cell where1 (box (suspV car env))) sto)]
                 [new-store2 (override-store (cell where2 (box (suspV cdr env))) new-store1)])
            (v*s (cellV (locationV where1) (locationV where2)) new-store2))]

    [carC  (exp)
           (let* ([result (interp exp env sto)]
                  [result-box (fetch (locationV-num (cellV-first (v*s-v result))) (v*s-s result))])
             (cond
               [(suspV? (unbox result-box)) 
                (let ([exp-interp (interp (suspV-exp (unbox result-box)) (suspV-env (unbox result-box)) (v*s-s result))])
                     (begin (set-box! result-box (v*s-v exp-interp))
                            exp-interp))]
               [else (v*s (unbox result-box) (v*s-s result))]))]
    
    [cdrC  (exp)
           (let* ([result (interp exp env sto)]
                  [result-box (fetch (locationV-num (cellV-second (v*s-v result))) (v*s-s result))])
             (cond
               [(suspV? (unbox result-box)) 
                (let ([exp-interp (interp (suspV-exp (unbox result-box)) (suspV-env (unbox result-box)) (v*s-s result))])
                     (begin (set-box! result-box (v*s-v exp-interp))
                            (v*s (unbox result-box) (v*s-s exp-interp))))]
               [else (v*s (unbox result-box) (v*s-s result))]))]
    
    ; Display values
    [displayC (exp) (let [(value (v*s-v (interp exp env sto)))]
                      (begin (print-value (v*s-v (interp exp env sto)))
                             (display ";")
                             (v*s value sto)))]
    ; Symbol
    [quoteC (sym) (v*s (quoteV sym) sto)]
    ; Null
    [nullC  () (v*s (nullV) sto)]
    ; Equal
    [equalC (x y)
            (let* ([exp1 (interp x env sto)] [exp2 (interp y env (v*s-s exp1))])
              (v*s (if (equal? (v*s-v exp1) (v*s-v exp2)) (boolV #t) (boolV #f)) (v*s-s exp2)))]
    ; Let* and Letrec
    [let*C (n1 a1 n2 a2 body)
           (interp (appC (lamC n1 (appC (lamC n2 body) a2)) a1)
                   env sto)]
    
    [letrecC (s f body) (type-case Result
                          (let* ([closD (closV 'empty (idC s) env)]
                                 [where (new-loc)]
                                 [func  (interp f (extend-env (bind s where) env) (override-store (cell where (box closD)) sto))])                                     
                            (interp body (extend-env (bind s where) env) (override-store (cell where (box (v*s-v func))) sto)))
                          [v*s (v-b s-b)
                               (v*s v-b s-b)])]
    ))

; Parser
(define (parse [s : s-expression]) : ExprS
  (cond
    [(s-exp-number? s) (numS (s-exp->number s))]
    [(s-exp-symbol? s) (idS (s-exp->symbol s))]
    [(s-exp-list? s)
     (let ([sl (s-exp->list s)])
       (if (empty? sl)
           (nullS)
           (case (s-exp->symbol (first sl))
             [(+) (plusS (parse (second sl)) (parse (third sl)))]
             [(*) (multS (parse (second sl)) (parse (third sl)))]
             [(-) (bminusS (parse (second sl)) (parse (third sl)))]
             [(~) (uminusS (parse (second sl)))]
             [(lambda) (lamS (s-exp->symbol (second sl)) (parse (third sl)))] ; definição
             [(call) (appS (parse (second sl)) (parse (third sl)))]
             [(if) (ifS (parse (second sl)) (parse (third sl)) (parse (fourth sl)))]
             [(seq) (seqS (parse (second sl)) (parse (third sl)))]
             [(:=) (setS (s-exp->symbol (second sl)) (parse (third sl)))]
             [(let) (letS (s-exp->symbol (first (s-exp->list (first (s-exp->list (second sl))))))
                          (parse (second (s-exp->list (first (s-exp->list (second sl))))))
                          (parse (third sl)))]
             [(cons) (consS (parse (second sl)) (parse (third sl)))]
             [(car) (carS (parse (second sl)))]
             [(cdr) (cdrS (parse (second sl)))]
             [(display)(displayS (parse (second sl)))]
             [(quote) (quoteS (s-exp->symbol (second sl)))]
             [(equal?) (equalS (parse (second sl)) (parse (third sl)))]
             [(let*) (let*S (s-exp->symbol (first (s-exp->list (first (s-exp->list (second sl))))))
                        (parse (second (s-exp->list (first (s-exp->list (second sl))))))
                        (s-exp->symbol (first (s-exp->list (second (s-exp->list (second sl))))))
                        (parse (second (s-exp->list (second (s-exp->list (second sl))))))
                        (parse (third sl)))]
             [(letrec) (letrecS (s-exp->symbol (first (s-exp->list (first (s-exp->list (second sl))))))
                            (parse (second (s-exp->list (first (s-exp->list (second sl))))))
                            (parse (third sl)))]
             [else (error 'parse "invalid list input")])))]
    [else (error 'parse "invalid input")]))


; Facilitator
(define (interpS [s : s-expression]) (v*s-v (interp (desugar (parse s)) mt-env mt-store)))

; Printing
(define (print-value [value : Value ] ) : void
                      (type-case Value value
                        [numV  (n) (display n)]
                        [quoteV (symb) (display symb)]
                        [closV (arg body env)
                               (begin (display "<<")
                                      (display "lambda(")
                                      (display arg)
                                      (display ")")
                                      (display body)
                                      (display ";")
                                      (display env)
                                      (display ">>"))]
                        
                        [cellV (first second)
                               (begin (display "(")
                                      (display value)
                                      (display ")")
                                      )
                               ]
                        [nullV ()
                               (display '())]
                        [boolV (b)
                               (begin (display "boolean: ")
                                      (display b))]
                        [suspV (exp env)
                               (begin (display "<|")
                                      (display "susp(")
                                      (display exp)
                                      (display ")")
                                      (display " ; ")
                                      (display env)
                                      (display "|>"))]
                        [locationV (n)
                                   (begin (display "Location ")
                                          (display n))]))
(define (print-list cell) : void
  (begin 
         (print-value (cellV-first cell))
         (display " ")
         (let ([rest (cellV-second cell)])
           (type-case Value rest 
             [nullV () (display "") ]; null at the end of the list is not printed
             [cellV (first second) (print-list rest)]
             [else (begin (display ".")
                        (print-value (cellV-second cell)))]))
         )
  )

; Exemplos

;Esta cria uma funcao recursiva fun e imprime o valor do parametro a cada chamada.
 (interpS '(let ((fun ()))
              (seq (:= fun (lambda x (if x (seq (display x)
                                                (call fun (- x 1)))
                                         x)))
                   (call fun 8))))
(test (interpS '(equal? 0 1)) (boolV #f))
(test (interpS '(equal? 1 1)) (boolV #t))
(test (interpS '(equal? (lambda x (+ 0 x)) (lambda x (+ 1 x)))) (boolV #f))
(test (interpS '(equal? (lambda x (+ 1 x)) (lambda x (+ 1 x)))) (boolV #t))
(test (interpS '(let* ([x 10]  [y 20]) (+ x y))) (numV 30))
(test (interpS '(let* ((x 5) (y (+ x 10))) y)) (numV 15))
(test (interpS '(letrec ([fac (lambda n (if n (* n (call fac (- n 1))) 1))])
                  (call fac 5))) (numV 120))
(test (interpS '(letrec ([l (lambda n (cons n (call l (+ 1 n))))]) (car (cdr (cdr (call l 1)))))) (numV 3))