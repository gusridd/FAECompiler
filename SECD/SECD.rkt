#lang play

(require "../stack.rkt")

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

;; intermediate representation for functions and id's
(deftype Intermediate
  (bruijnFun body)
  (bruijnNumber n))


(defun (n-th list pos)
  (last (take list pos)))

(test (n-th (list 3 4 5) 1) 3)
(test (n-th (list 3 4 5) 2) 4)
(test (n-th (list 3 4 5) 3) 5)

;; run :: List[Instruction], List[Instructions] -> CONST
(defun (run ins-list stack env)
  #;(begin
      (display "\ninstructions\n")
      (print ins-list)
      (display "\nstack\n")
      (print stack)
      )
  (match ins-list
    ['() (first stack)]
    [(list (CONST n) tail-instructions ...) 
     (run tail-instructions (cons (CONST n) stack) env)]
    [(list (ADD) tail-instructions ...) (def (CONST n1) (first stack))
                                        (def (CONST n2) (second stack))
                                        (def new-stack (drop stack 2))
                                        (run tail-instructions (cons (CONST (+ n2 n1)) new-stack) env)]
    [(list (SUB) tail-instructions ...) (def (CONST n1) (first stack))
                                        (def (CONST n2) (second stack))
                                        (def new-stack (drop stack 2))
                                        (run tail-instructions (cons (CONST (- n2 n1)) new-stack) env)]
    
    [(list (ACCESS n) tail-instructions ...) (run tail-instructions 
                                                      (cons (n-th env n) stack) 
                                                      env)]
    [(list (LET) tail-instructions ...) (run tail-instructions 
                                                 (drop stack 1) 
                                                 (cons (first stack) env))]
    [(list (ENDLET) tail-instructions ...) (run tail-instructions
                                                    stack
                                                    (drop 1 env))]
    [(list (CLOSURE cp) tail-instructions ...) (run tail-instructions
                                                        (cons (closureV cp env) stack)
                                                        env)]
    [(list (APPLY) tail-instructions ...) (def v (first stack))
                                          (def (closureV cp ep) (second stack))
                                          (def s (drop stack 2))
                                          (run cp 
                                                   (append tail-instructions (cons env (cons s '())))
                                                   (cons v ep))]
    [(list (RETURN) tail-instructions ...) (def v (first stack))
                                           (def cp (second stack))
                                           (def ep (third stack))
                                           (def s (drop stack 3))
                                           (run cp
                                                    (cons v s)
                                                    ep)]))

(defun (machine ins-list)
  (run ins-list '() '()))


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

(test (machine (list (CLOSURE (list (ACCESS 1)
                                    (CONST 1)
                                    (ADD)
                                    (RETURN)))
                     (CONST 2)
                     (APPLY)))
      (CONST 3))

;;;;;;;;;;;;;;;;;;;;;;;
;; Language definition
;;;;;;;;;;;;;;;;;;;;;;;

#|
<expr> ::= <num>
         | {+ <expr> <expr>}
         | {- <expr> <expr>}
|#
(deftype Expr
  (num n)
  (add l r)
  (sub l r)
  (id s)
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
    [(list 'fun (list x) b) (fun x (parse b))]
    [(list f a) (app (parse f) (parse a))]))


(defun (debruijn expr)
  (letrec ([auxbruijn (λ(e bid lvl)
                        (match e
                          [(num n) (num n)]
                          [(id x) (if (eq? x bid)
                                      (bruijnNumber lvl)
                                      (id x))]
                          [(add l r) (add (auxbruijn l bid lvl) (auxbruijn r bid lvl))]
                          [(sub l r) (sub (auxbruijn l bid lvl) (auxbruijn r bid lvl))]
                          [(app a b) (app (auxbruijn a bid lvl) (auxbruijn b bid lvl))]
                          [(fun id body) (if (eq? id bid)
                                             (debruijn (fun id body))
                                             (debruijn (fun id (auxbruijn body bid (+ 1 lvl)))))]
                          [(bruijnFun body) (bruijnFun (auxbruijn body bid (+ 1 lvl)))]
                          [(bruijnNumber n) (bruijnNumber n)]
                          ))])
    (match expr
      [(num n) (num n)]
      [(id x) (id x)]
      [(add l r) (add (debruijn l) (debruijn r))]
      [(sub l r) (sub (debruijn l) (debruijn r))]
      [(app a b) (app (debruijn a) (debruijn b))]
      [(fun id body) (bruijnFun (auxbruijn body id 1))])))

(test (debruijn (parse '{+ 1 2}))
      (parse '{+ 1 2}))

(test (debruijn (parse '{fun {x} x}))
      (bruijnFun (bruijnNumber 1)))

(test (debruijn (parse '{fun {x} 
                             {fun {y} x}}))
      (bruijnFun (bruijnFun (bruijnNumber 2))))

(test (debruijn (parse '{fun {x}
                             {fun {x} x}}))
      (bruijnFun (bruijnFun (bruijnNumber 1))))

(test (debruijn (parse '{{fun {x} {+ x 1}} 2}))
      (app (bruijnFun (add (bruijnNumber 1) (num 1))) (num 2)))

;; compile :: Expr -> List[Instruction]
(defun (compile expr)
  (letrec ([e (debruijn (parse expr))]
           [comp (λ(e)
                   (match e
                     [(num n) (list (CONST n))]
                     [(add l r) (append (comp l) (comp r) (list (ADD)))]
                     [(sub l r) (append (comp l) (comp r) (list (SUB)))]
                     [(app a b) (append (comp a) (comp b) (list (APPLY)))]
                     [(fun id body) (list (CLOSURE (append (comp body) (list (RETURN)))))]
                     [(bruijnFun body) (list (CLOSURE (append (comp body) (list (RETURN)))))]
                     [(bruijnNumber n) (list (ACCESS n))]
                     ))])
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



