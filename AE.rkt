#lang play

(require "stack.rkt")

(print-only-errors #t)

;;;;;;;;;;;;;;;;;;;;;;;
;; Machine definition
;;;;;;;;;;;;;;;;;;;;;;;

(deftype Instruction
  (CONST n)
  (ADD)
  (SUB))

;; Debug function for the machine
(defun (debug-run ins-list stack)
  (begin
    (display "\ninstructions: ")
    (print ins-list)
    (display "\nstack: ")
    (stack-debug stack)
    (display "\n")))

;; run :: List[Instruction], Stack -> CONST
;; SIGFAULT: thrown when the underlying constructs fail
;; CORRUPT_ENDING_STATE: thrown when the machine ends with a stack with size different from one 
(defun (run ins-list stack)
  (let ([non-local-exn? (λ(ex) (not (string=? (exn-message ex) 
                                              "CORRUPT_ENDING_STATE")))]
        [fault (λ(ex) (error "SIGFAULT"))])
    ;(debug-run ins-list stack)
    (match ins-list
      ['() (if (= 1 (stack-size stack))
               (stack-top stack)
               (error "CORRUPT_ENDING_STATE"))]
      [(list (CONST n) tail-instructions ...)
       (run tail-instructions (stack-push stack (CONST n)))]
      [(list (ADD) tail-instructions ...)
       (with-handlers ([non-local-exn? fault])
         (def (CONST n1) (stack-top stack))
         (def (CONST n2) (stack-top (stack-pop stack)))
         (def new-stack (stack-pop (stack-pop stack)))
         (run tail-instructions (stack-push new-stack (CONST (+ n2 n1)))))]
      [(list (SUB) tail-instructions ...)
       (with-handlers ([non-local-exn? fault])
         (def (CONST n1) (stack-top stack))
         (def (CONST n2) (stack-top (stack-pop stack)))
         (def new-stack (stack-pop (stack-pop stack)))
         (run tail-instructions (stack-push new-stack (CONST (- n2 n1)))))])))

(test (run (list (CONST 5)) 
           (stack-init))
      (CONST 5))

(test (run (list (CONST 1)
                 (CONST 2)
                 (ADD)) 
           (stack-init))
      (CONST 3))

(test (run (list (CONST 5)
                 (CONST 1)
                 (CONST 2)
                 (ADD)
                 (SUB)) 
           (stack-init)) 
      (CONST 2))

(test/exn (run (list (CONST 5)
                     (CONST 2)) 
               (stack-init)) "CORRUPT_ENDING_STATE")

(test/exn (run (list (CONST 5)
                     (CONST 2)
                     (CONST 2)
                     (ADD)) 
               (stack-init)) "CORRUPT_ENDING_STATE")

(test/exn (run (list (CONST 5)
                     (CONST 2)
                     (CONST 2)
                     (SUB)) 
               (stack-init)) "CORRUPT_ENDING_STATE")

(test/exn (run (list (ADD)) (stack-init)) "SIGFAULT")
(test/exn (run (list (CONST 1) (ADD)) (stack-init)) "SIGFAULT")
(test/exn (run (list (SUB)) (stack-init)) "SIGFAULT")
(test/exn (run (list (CONST 1) (SUB)) (stack-init)) "SIGFAULT")
(test/exn (run (list (CONST 1) (CONST 4) (SUB) (ADD)) (stack-init)) "SIGFAULT")

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





