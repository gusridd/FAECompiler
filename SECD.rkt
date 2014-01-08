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
  (brujinFun body)
  (brujinNumber n))

;; run :: List[Instruction], Stack[Instructions], List -> CONST
(defun (run ins-list stack env)
  #;(begin
      (display "\ninstructions\n")
      (print ins-list)
      (display "\nstack\n")
      (print stack)
      )
  (match ins-list
    ['() (stack-top stack)]
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
                                   (drop 1 env))]
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
                                   ep)]))

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


(defun (DeBrujin expr)
  (letrec ([auxBrujin (λ(e bid lvl)
                        (match e
                          [(num n) (num n)]
                          [(id x) (if (eq? x bid)
                                      (brujinNumber lvl)
                                      (id x))]
                          [(add l r) (add (auxBrujin l bid lvl) (auxBrujin r bid lvl))]
                          [(sub l r) (sub (auxBrujin l bid lvl) (auxBrujin r bid lvl))]
                          [(app a b) (app (auxBrujin a bid lvl) (auxBrujin b bid lvl))]
                          [(fun id body) (if (eq? id bid)
                                             (DeBrujin (fun id body))
                                             (DeBrujin (fun id (auxBrujin body bid (+ 1 lvl)))))]
                          [(brujinFun body) (brujinFun (auxBrujin body bid (+ 1 lvl)))]
                          [(brujinNumber n) (brujinNumber n)]
                          ))])
    (match expr
      [(num n) (num n)]
      [(id x) (id x)]
      [(add l r) (add (DeBrujin l) (DeBrujin r))]
      [(sub l r) (sub (DeBrujin l) (DeBrujin r))]
      [(app a b) (app (DeBrujin a) (DeBrujin b))]
      [(fun id body) (brujinFun (auxBrujin body id 1))])))

(test (DeBrujin (parse '{+ 1 2}))
      (parse '{+ 1 2}))

(test (DeBrujin (parse '{fun {x} x}))
      (brujinFun (brujinNumber 1)))

(test (DeBrujin (parse '{fun {x} 
                             {fun {y} x}}))
      (brujinFun (brujinFun (brujinNumber 2))))

(test (DeBrujin (parse '{fun {x}
                             {fun {x} x}}))
      (brujinFun (brujinFun (brujinNumber 1))))

(test (DeBrujin (parse '{{fun {x} {+ x 1}} 2}))
      (app (brujinFun (add (brujinNumber 1) (num 1))) (num 2)))

;; compile :: Expr -> List[Instruction]
(defun (compile expr)
  (letrec ([e (DeBrujin (parse expr))]
           [comp (λ(e)
                   (match e
                     [(num n) (list (CONST n))]
                     [(add l r) (append (comp l) (comp r) (list (ADD)))]
                     [(sub l r) (append (comp l) (comp r) (list (SUB)))]
                     [(app a b) (append (comp a) (comp b) (list (APPLY)))]
                     [(fun id body) (list (CLOSURE (append (comp body) (list (RETURN)))))]
                     [(brujinFun body) (list (CLOSURE (append (comp body) (list (RETURN)))))]
                     [(brujinNumber n) (list (ACCESS n))]
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



