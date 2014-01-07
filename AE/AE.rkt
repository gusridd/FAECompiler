#lang play

(require "../stack.rkt")

(print-only-errors #t)

;;;;;;;;;;;;;;;;;;;;;;;
;; Machine definition
;;;;;;;;;;;;;;;;;;;;;;;

(deftype Instruction
  (CONST n)
  (ADD)
  (SUB))

;; run :: List[Instruction], List[Instructions] -> CONST
(defun (run ins-list stack)
  #;(begin
      (display "\ninstructions\n")
      (print ins-list)
      (display "\nstack\n")
      (print stack)
      )
  (match ins-list
    ['() (stack-top stack)]
    [(list (CONST n) tail-instructions ...)
     (run tail-instructions (stack-push stack (CONST n)))]
    [(list (ADD) tail-instructions ...) (def (CONST n1) (stack-top stack))
                                        (def (CONST n2) (stack-top (stack-pop stack)))
                                        (def new-stack (stack-pop (stack-pop stack)))
                                        (run tail-instructions (stack-push new-stack (CONST (+ n2 n1))))]
    [(list (SUB) tail-instructions ...) (def (CONST n1) (stack-top stack))
                                        (def (CONST n2) (stack-top (stack-pop stack)))
                                        (def new-stack (stack-pop (stack-pop stack)))
                                        (run tail-instructions (stack-push new-stack (CONST (- n2 n1))))]))

(test (run (list (CONST 5)) (stack-init))
      (CONST 5))

(test (run (list (CONST 1)
                 (CONST 2)
                 (ADD)) (stack-init))
      (CONST 3))

(test (run (list (CONST 5)
                 (CONST 1)
                 (CONST 2)
                 (ADD)
                 (SUB)) (stack-init)) 
      (CONST 2))

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
  (sub l r))

;; parse :: s-expr -> Expr
#| where
   <s-expr> ::= <num>
              | (list '+ <s-expr> <s-expr>)
              | (list '- <s-expr> <s-expr>)
|#
(defun (parse s-expr)
  (match s-expr
    [(? number?) (num s-expr)]
    [(list '+ l r) (add (parse l) (parse r))]
    [(list '- l r) (sub (parse l) (parse r))]))

;; compile :: Expr -> List[Instruction]
(defun (compile expr)
  (match expr
    [(num n) (list (CONST n))]
    [(add l r) (append (compile l) (compile r) (list (ADD)))]
    [(sub l r) (append (compile l) (compile r) (list (SUB)))]))


(test (compile (parse '3))
      (list (CONST 3)))

(test (compile (parse '{+ 3 2}))
      (list (CONST 3)
            (CONST 2)
            (ADD)))

(test (compile (parse '{- 3 2}))
      (list (CONST 3)
            (CONST 2)
            (SUB)))

(test (compile (parse '{- 5 {+ 1 2}}))
      (list (CONST 5)
            (CONST 1)
            (CONST 2)
            (ADD)
            (SUB)))





