#lang play

(require "stack.rkt")

(print-only-errors #t)

;;;;;;;;;;;;;;;;;;;;;;;
;; Machine definition
;;;;;;;;;;;;;;;;;;;;;;;

(deftype Instruction
  (CONST n)
  (ADD)
  (SUB)
  (ACCESS n)
  (CLOSURE c)
  (LET)
  (ENDLET)
  (APPLY)
  (RETURN))

;; values
(deftype Val
  (numV n)
  (closureV body env))

;; intermediate
(deftype Intermediate
  (bruijnFun body)
  (bruijnNumber n))

;; Debug function for the machine
(defun (debug-run ins-list stack)
  (begin
    (display "\ninstructions: ")
    (print ins-list)
    (display "\nstack: ")
    (stack-debug stack)
    (display "\n")))

;; run :: List[Instruction], Stack[Instructions], List -> CONST
(defun (run ins-list stack env)
  ;(debug-run ins-list stack)
  (match ins-list
    ['() (if (= 1 (stack-size stack))
             (stack-top stack)
             (error "CORRUPT_ENDING_STATE"))]
    [_ (let ([non-local-exn? (λ(ex) (not (string=? (exn-message ex) 
                                                   "CORRUPT_ENDING_STATE")))]
             [fault (λ(ex) 
                      ;(print (exn-message ex)) 
                      (error "SIGFAULT"))])
         (with-handlers ([non-local-exn? fault])
           (match ins-list
             [(list (CONST n) tail ...) 
              (run tail (stack-push stack (CONST n)) env)]
             [(list (ADD) tail ...) (def (CONST n1) (stack-top stack))
                                    (def (CONST n2) (stack-top (stack-pop stack)))
                                    (def new-stack (stack-pop (stack-pop stack)))
                                    (run tail (stack-push new-stack (CONST (+ n2 n1))) env)]
             [(list (SUB) tail ...) (def (CONST n1) (stack-top stack))
                                    (def (CONST n2) (stack-top (stack-pop stack)))
                                    (def new-stack (stack-pop (stack-pop stack)))
                                    (run tail (stack-push new-stack (CONST (- n2 n1))) env)]
             
             [(list (ACCESS n) tail ...) (run tail 
                                              (stack-push stack (list-ref env (- n 1))) 
                                              env)]
             [(list (LET) tail ...) (run tail 
                                         (stack-pop stack) 
                                         (cons (stack-top stack) env))]
             [(list (ENDLET) tail ...) (run tail
                                            stack
                                            (drop env 1))]
             [(list (CLOSURE cp) tail ...) (run tail
                                                (stack-push stack (closureV cp env))
                                                env)]
             [(list (APPLY) tail ...) (def v (stack-top stack))
                                      (def (closureV cp ep) (stack-top (stack-pop stack)))
                                      (def s (stack-pop (stack-pop stack)))
                                      (run cp 
                                           (stack-push (stack-push s env) tail)
                                           (cons v ep))]
             [(list (RETURN) tail ...) (def return-value (stack-top stack))
                                       (def cp (stack-top (stack-pop stack)))
                                       (def ep (stack-top (stack-pop (stack-pop stack))))
                                       (def s (stack-pop (stack-pop (stack-pop stack))))
                                       (run cp
                                            (stack-push s return-value)
                                            ep)])))]))

(defun (machine ins-list)
  (run ins-list (stack-init) '()))


(test (machine (list (CONST 5)))
      (CONST 5))

(test (machine (list (CONST 1)
                     (CONST 2)
                     (ADD)))
      (CONST 3))

(test (machine (list (CONST 5)
                     (CONST 1)
                     (CONST 2)
                     (ADD)
                     (SUB))) 
      (CONST 2))

(test (machine (list (CONST 3) 
                     (LET) 
                     (ACCESS 1) 
                     (ENDLET)))
      (CONST 3))

(test/exn (machine (list (CONST 5)
                         (CONST 2))) "CORRUPT_ENDING_STATE")

(test/exn (machine (list (CONST 5)
                         (CONST 2)
                         (CONST 2)
                         (ADD))) "CORRUPT_ENDING_STATE")

(test/exn (machine (list (CONST 5)
                         (CONST 2)
                         (CONST 2)
                         (SUB))) "CORRUPT_ENDING_STATE")

(test/exn (machine (list (ADD))) "SIGFAULT")
(test/exn (machine (list (CONST 1) (ADD))) "SIGFAULT")
(test/exn (machine (list (SUB))) "SIGFAULT")
(test/exn (machine (list (CONST 1) (SUB))) "SIGFAULT")
(test/exn (machine (list (CONST 1) (CONST 4) (SUB) (ADD))) "SIGFAULT")

(test (machine (list (CLOSURE (list (ACCESS 1)
                                    (CONST 1)
                                    (ADD)
                                    (RETURN)))
                     (CONST 2)
                     (APPLY)))
      (CONST 3))

(test (machine (list (CLOSURE (list (ACCESS 1) (CONST 1) (APPLY) (RETURN))) 
                     (CLOSURE (list (ACCESS 1) (CONST 1) (ADD) (RETURN))) 
                     (APPLY))) 
      (CONST 2))



;;;;;;;;;;;;;;;;;;;;;;;
;; Language definition
;;;;;;;;;;;;;;;;;;;;;;;

#|
<expr> ::= <num>
         | {+ <expr> <expr>}
         | {- <expr> <expr>}
         | {with <expr> in <expr>}
         | {fun {<id>} <expr>}
         | {<expr> <expr>}
|#
(deftype Expr
  (num n)
  (add l r)
  (sub l r)
  (id s)
  (with a b)
  (acc n)
  (app fun-expr arg-expr)
  (fun id body))

;; parse :: s-expr -> Expr
#| where
   <s-expr> ::= <num>
              | (list '+ <s-expr> <s-expr>)
              | (list '- <s-expr> <s-expr>)
|#
(defun (parse s-expr)
  (match s-expr
    [(? number?) (num s-expr)]
    [(? symbol?) (id s-expr)]
    [(list '+ l r) (add (parse l) (parse r))]
    [(list '- l r) (sub (parse l) (parse r))]
    [(list 'with a 'in b) (with (parse a) (parse b))]
    [(list 'acc n) (acc n)]
    [(list 'fun (list x) b) (fun x (parse b))]
    [(list f a) (app (parse f) (parse a))]))


(defun (deBruijn expr)
  (letrec ([auxBruijn (λ(e bid lvl)
                        (match e
                          [(num n) (num n)]
                          [(acc n) (acc n)]
                          [(id x) (if (eq? x bid)
                                      (bruijnNumber lvl)
                                      (id x))]
                          [(add l r) (add (auxBruijn l bid lvl) (auxBruijn r bid lvl))]
                          [(sub l r) (sub (auxBruijn l bid lvl) (auxBruijn r bid lvl))]
                          [(app a b) (app (auxBruijn a bid lvl) (auxBruijn b bid lvl))]
                          [(fun id body) (if (eq? id bid)
                                             (deBruijn (fun id body))
                                             (deBruijn (fun id (auxBruijn body bid (+ 1 lvl)))))]
                          [(bruijnFun body) (bruijnFun (auxBruijn body bid (+ 1 lvl)))]
                          [(bruijnNumber n) (bruijnNumber n)]
                          ))])
    (match expr
      [(num n) (num n)]
      [(acc n) (acc n)]
      [(id x) (id x)]
      [(add l r) (add (deBruijn l) (deBruijn r))]
      [(sub l r) (sub (deBruijn l) (deBruijn r))]
      [(with a b) (with (deBruijn a) (deBruijn b))]
      [(app a b) (app (deBruijn a) (deBruijn b))]
      [(fun id body) (bruijnFun (auxBruijn body id 1))])))

(test (deBruijn (parse '{+ 1 2}))
      (parse '{+ 1 2}))

(test (deBruijn (parse '{fun {x} x}))
      (bruijnFun (bruijnNumber 1)))

(test (deBruijn (parse '{fun {x} 
                             {fun {y} x}}))
      (bruijnFun (bruijnFun (bruijnNumber 2))))

(test (deBruijn (parse '{fun {x}
                             {fun {x} x}}))
      (bruijnFun (bruijnFun (bruijnNumber 1))))

(test (deBruijn (parse '{{fun {x} {+ x 1}} 2}))
      (app (bruijnFun (add (bruijnNumber 1) (num 1))) (num 2)))

;; compile :: Expr -> List[Instruction]
(defun (compile expr)
  (letrec ([e (deBruijn (parse expr))]
           [comp (λ(e)
                   (match e
                     [(num n) (list (CONST n))]
                     [(acc n) (list (ACCESS n))]
                     [(add l r) (append (comp l) (comp r) (list (ADD)))]
                     [(sub l r) (append (comp l) (comp r) (list (SUB)))]
                     [(with a b) (append (comp a) (list (LET)) (comp b) (list (ENDLET)))]
                     [(app a b) (append (comp a) (comp b) (list (APPLY)))]
                     [(fun id body) (list (CLOSURE (append (comp body) (list (RETURN)))))]
                     [(bruijnFun body) (list (CLOSURE (append (comp body) (list (RETURN)))))]
                     [(bruijnNumber n) (list (ACCESS n))]))])
    (comp e)))

(test (compile '3)
      (list (CONST 3)))

(test (compile '{+ 3 2})
      (list (CONST 3)
            (CONST 2)
            (ADD)))

(test (compile '{- 3 2})
      (list (CONST 3)
            (CONST 2)
            (SUB)))

(test (compile '{- 5 {+ 1 2}})
      (list (CONST 5)
            (CONST 1)
            (CONST 2)
            (ADD)
            (SUB)))

(test (compile '{{fun {x} {+ x 1}} 2})
      (list (CLOSURE (list (ACCESS 1)
                           (CONST 1)
                           (ADD)
                           (RETURN)))
            (CONST 2)
            (APPLY)))

(test (compile '{{fun {f} {f 1}} {fun {x} {+ x 1}}})
      (list (CLOSURE (list (ACCESS 1) (CONST 1) (APPLY) (RETURN))) 
            (CLOSURE (list (ACCESS 1) (CONST 1) (ADD) (RETURN))) 
            (APPLY)))

(test (compile '{with 3 in {acc 1}})
      (list (CONST 3) 
            (LET) 
            (ACCESS 1) 
            (ENDLET)))



