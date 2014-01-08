#lang play

(print-only-errors #t)

;; ADT STACK

(deftype Stack
  (Stacked value next)
  (EmptyStack))

;; stack-init :: -> Stack
(defun (stack-init)
  (EmptyStack))

(test (stack-init) (EmptyStack))

;; stack-pop :: Stack -> Stack
(defun (stack-pop s)
  (match s
    [(Stacked v n) n]
    [(EmptyStack) (error "stack-pop to an EmptyStack")]))

(test (stack-pop (Stacked 3 (EmptyStack))) 
      (EmptyStack))
(test (stack-pop (Stacked 3 (Stacked 4 (EmptyStack)))) 
      (Stacked 4 (EmptyStack)))
(test/exn (stack-pop (stack-init)) "stack-pop to an EmptyStack")

;; stack-top :: Stack -> V
(defun (stack-top s)
  (match s
    [(Stacked v n) v]
    [(EmptyStack) (error "stack-top to an EmptyStack")]))

(test (stack-top (Stacked 3 (EmptyStack))) 3)
(test (stack-top (Stacked 8 (Stacked 4 (EmptyStack)))) 8)
(test/exn (stack-top (EmptyStack)) "stack-top to an EmptyStack")

;; stack-push :: Stack, V -> Stack
(defun (stack-push s v)
  (Stacked v s))

;; stack-empty :: Stack -> Boolean
(defun (stack-empty? s)
  (match s
    [(EmptyStack) #t]
    [(Stacked v n) #f]))

(test (stack-empty? (EmptyStack)) #t)
(test (stack-empty? (Stacked 3(EmptyStack))) #f)

;; stack-size :: Stack -> Int
(defun (stack-size s)
  (letrec ([sstr (λ(s c)
                   (match s
                     [(EmptyStack) c]
                     [(Stacked v n) (sstr n (+ 1 c))]))])
    (sstr s 0)))

(test (stack-size (EmptyStack)) 0)
(test (stack-size (Stacked 8 (Stacked 4 (EmptyStack)))) 2)

;; stack-to-list :: Stack -> List[V]
(defun (stack-to-list stack)
  (match stack
    [(EmptyStack) '()]
    [(Stacked v next) (cons v (stack-to-list next))]))

(test (stack-to-list (EmptyStack)) '())
(test (stack-to-list (Stacked 3 (Stacked 8 (Stacked 1 (EmptyStack))))) (list 3 8 1))

;; list-to-stack :: List[V] -> Stack
(defun (list-to-stack list)
  (match list
    ['() (EmptyStack)]
    [(cons h t) (Stacked h (list-to-stack t))]))

(test (list-to-stack '()) (EmptyStack))
(test (list-to-stack (list 3 8 1)) (Stacked 3 (Stacked 8 (Stacked 1 (EmptyStack)))))

;; stack-debug :: Stack -> void
(defun (stack-debug stack)
  (letrec ([collectString (λ(s)
                            (match s
                              [(EmptyStack) ""]
                              [(Stacked v next) (string-append (~v v) " ] " (collectString next))]))])
    (display (collectString stack))))

