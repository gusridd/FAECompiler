#lang play

(require "stack.rkt")

(print-only-errors #t)

;;;;;;;;;;;;;;;;;;;;;;;
;; Machine definition
;;;;;;;;;;;;;;;;;;;;;;;

(deftype Instruction
  (INT_CONST n)
  (CLOSURE_CONST name)
  (ADD)
  (SUB)
  (ACCESS n)
  (CLOSURE ins)
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
             [(list (INT_CONST n) tail ...) 
              (run tail (stack-push stack (INT_CONST n)) env)]
             [(list (ADD) tail ...) (def (INT_CONST n1) (stack-top stack))
                                    (def (INT_CONST n2) (stack-top (stack-pop stack)))
                                    (def new-stack (stack-pop (stack-pop stack)))
                                    (run tail (stack-push new-stack (INT_CONST (+ n2 n1))) env)]
             [(list (SUB) tail ...) (def (INT_CONST n1) (stack-top stack))
                                    (def (INT_CONST n2) (stack-top (stack-pop stack)))
                                    (def new-stack (stack-pop (stack-pop stack)))
                                    (run tail (stack-push new-stack (INT_CONST (- n2 n1))) env)]
             
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


(test (machine (list (INT_CONST 5)))
      (INT_CONST 5))

(test (machine (list (INT_CONST 1)
                     (INT_CONST 2)
                     (ADD)))
      (INT_CONST 3))

(test (machine (list (INT_CONST 5)
                     (INT_CONST 1)
                     (INT_CONST 2)
                     (ADD)
                     (SUB))) 
      (INT_CONST 2))

(test (machine (list (INT_CONST 3) 
                     (LET) 
                     (ACCESS 1) 
                     (ENDLET)))
      (INT_CONST 3))



(test/exn (machine (list (INT_CONST 5)
                         (INT_CONST 2))) "CORRUPT_ENDING_STATE")

(test/exn (machine (list (INT_CONST 5)
                         (INT_CONST 2)
                         (INT_CONST 2)
                         (ADD))) "CORRUPT_ENDING_STATE")

(test/exn (machine (list (INT_CONST 5)
                         (INT_CONST 2)
                         (INT_CONST 2)
                         (SUB))) "CORRUPT_ENDING_STATE")

(test/exn (machine (list (ADD))) "SIGFAULT")
(test/exn (machine (list (INT_CONST 1) (ADD))) "SIGFAULT")
(test/exn (machine (list (SUB))) "SIGFAULT")
(test/exn (machine (list (INT_CONST 1) (SUB))) "SIGFAULT")
(test/exn (machine (list (INT_CONST 1) (INT_CONST 4) (SUB) (ADD))) "SIGFAULT")

(test/exn (machine (list
                    (CLOSURE (list (ACCESS 1) 
                                   (ACCESS 1) 
                                   (INT_CONST 1) 
                                   (APPLY) 
                                   (APPLY) 
                                   (RETURN)))
                    (LET)
                    (ACCESS 1)
                    (ACCESS 1)
                    (INT_CONST 1)
                    (APPLY)
                    (APPLY)
                    (ENDLET))) "SIGFAULT")

(test (machine (list (CLOSURE (list (ACCESS 1)
                                    (INT_CONST 1)
                                    (ADD)
                                    (RETURN)))
                     (INT_CONST 2)
                     (APPLY)))
      (INT_CONST 3))

(test (machine (list (CLOSURE (list (ACCESS 1) (INT_CONST 1) (APPLY) (RETURN))) 
                     (CLOSURE (list (ACCESS 1) (INT_CONST 1) (ADD) (RETURN))) 
                     (APPLY))) 
      (INT_CONST 2))



;;;;;;;;;;;;;;;;;;;;;;;
;; Language definition
;;;;;;;;;;;;;;;;;;;;;;;

#|
<expr> ::= <num>
         | <id>
         | {acc <num>}
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

;; deBruijn :: Expr -> Expr + Intermediate
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
      (add (num 1) (num 2)))

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

(test (deBruijn (parse '{with {fun {f} {f {f 1}}} in
                              {{acc 1} {{acc 1} 1}}}))
      (with
       (bruijnFun (app (bruijnNumber 1) (app (bruijnNumber 1) (num 1))))
       (app (acc 1) (app (acc 1) (num 1)))))

;; compile :: Expr -> List[Instruction]
(defun (compile expr)
  (letrec ([e (deBruijn (parse expr))]
           [comp (λ(e)
                   (match e
                     [(num n) (list (INT_CONST n))]
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
      (list (INT_CONST 3)))

(test (compile '{+ 3 2})
      (list (INT_CONST 3)
            (INT_CONST 2)
            (ADD)))

(test (compile '{- 3 2})
      (list (INT_CONST 3)
            (INT_CONST 2)
            (SUB)))

(test (compile '{- 5 {+ 1 2}})
      (list (INT_CONST 5)
            (INT_CONST 1)
            (INT_CONST 2)
            (ADD)
            (SUB)))

(test (compile '{{fun {x} {+ x 1}} 2})
      (list (CLOSURE (list (ACCESS 1)
                           (INT_CONST 1)
                           (ADD)
                           (RETURN)))
            (INT_CONST 2)
            (APPLY)))

(test (compile '{{fun {f} {f 1}} {fun {x} {+ x 1}}})
      (list (CLOSURE (list (ACCESS 1) (INT_CONST 1) (APPLY) (RETURN))) 
            (CLOSURE (list (ACCESS 1) (INT_CONST 1) (ADD) (RETURN))) 
            (APPLY)))

(test (compile '{with 3 in {acc 1}})
      (list (INT_CONST 3) 
            (LET) 
            (ACCESS 1) 
            (ENDLET)))

(test (compile '{with {fun {f} {f {f 1}}} in
                      {{acc 1} {{acc 1} 1}}})
      (list
       (CLOSURE (list (ACCESS 1) 
                      (ACCESS 1) 
                      (INT_CONST 1) 
                      (APPLY) 
                      (APPLY) 
                      (RETURN)))
       (LET)
       (ACCESS 1)
       (ACCESS 1)
       (INT_CONST 1)
       (APPLY)
       (APPLY)
       (ENDLET)))


(defun (i-map fun list)
  (letrec ([indexed (λ(l i)
                      (match l
                        ['() '()]
                        [(cons h t) (cons (fun h i) (indexed t (+ 1 i)))]))])
    (indexed list 1)))

(test (i-map (λ(e i) i) (list)) (list))
(test (i-map (λ(e i) i) (list 1 1 1 1 1 1 1)) (list 1 2 3 4 5 6 7))
(test (i-map (λ(e i) (cons e i)) (list 1 1 1 1 1 1 1)) '((1 . 1) (1 . 2) (1 . 3) (1 . 4) (1 . 5) (1 . 6) (1 . 7)))


;; collectClosures :: List[Expr] -> List[Closure]
(defun (collectClosures l)
  (apply append (map (λ(c) 
                       (cons c (collectClosures (CLOSURE-ins c)))) 
                     (filter CLOSURE? l))))


(test (collectClosures (list (CLOSURE (list (CLOSURE (list (INT_CONST 1) 
                                                           (INT_CONST 2) 
                                                           (ADD) 
                                                           (RETURN))) 
                                            (RETURN))) 
                             (INT_CONST 3) 
                             (APPLY)))
      (list (CLOSURE (list (CLOSURE (list (INT_CONST 1) 
                                          (INT_CONST 2) 
                                          (ADD) 
                                          (RETURN))) 
                           (RETURN)))
            (CLOSURE (list (INT_CONST 1) 
                           (INT_CONST 2) 
                           (ADD) 
                           (RETURN)))))

;;
;; $t0 -> first parameter
;; $t1 -> second parameter

;; $t3 -> temp usage
;; $t4 -> temp usage
;; $t5 -> temp usage
;; $t6 -> temp usage
;; $t7 -> temp usage

;; $t9 RESERVED for storing $ra between calls to copy

;; 0($fp) -> return address
;; 4($fp) -> old frame pointer 
;; 8($fp) -> env[0] (new-arg)
;; 12($fp) -> env[1]


;; rules
;; before function aplication the environment must be copied into the stack, save the frame pointer to the top and the new parameter be placed at top. Refresh the fp and the sp


(defun (spim-compile ins-list)
  (letrec ([inline (λ(l) (apply string-append (map (λ(s) (string-append "\t" s "\n")) 
                                                   l)))]
           [closureHash (make-hash (i-map (λ(e i) 
                                            (cons e (string-append "fun" (~a i)))) 
                                          (collectClosures ins-list)))]
           [replaceClosures (λ(l) (map (λ(e) (match e
                                               [(CLOSURE ins) (CLOSURE_CONST (hash-ref closureHash e))]
                                               [_ e])) l))]
           [replacedClosureHash (make-hash (hash-map closureHash 
                                                     (λ(k v)
                                                       (match k
                                                         [(CLOSURE ins) (cons (CLOSURE (replaceClosures ins)) v)]))))]
           [constants (apply string-append
                             (map (λ(c) (let ([num (~a (INT_CONST-n c))])
                                          (string-append "int" num ":\t.word\t" num "\n")))
                                  (remove-duplicates (append (filter (λ(e)(INT_CONST? e)) ins-list)
                                                             (apply append (hash-map replacedClosureHash
                                                                                     (λ(k v)
                                                                                       (match k
                                                                                         [(CLOSURE ins) (filter INT_CONST? ins)]))))))))]
           [captureEnvPrimitive (inline (list "\ncaptureEnv:"
                                              "lw $t0, 12($fp) \t\t# t0 = env-size"
                                              "li $t1, 4"
                                              "move $t3, $t0"
                                              "mult $t3, $t1"
                                              "mflo $t3 \t\t# t3 = t3 * 4 (alignment)"
                                              "sub $t3, $sp, $t3 \t# t3 is the position for the env-size"
                                              "# sw $t0, ($t3)"
                                              "move $sp, $t3"
                                              "addi $sp, $sp, -4 \t# position sp to receive function pointer"
                                              "move $t4, $t3 \t# position t4 to receive the copy"
                                              "addi $t3, $fp, 12 \t# position t3 to source the copy"
                                              "addi $t0, $t0, 1 \t# to copy the env-size"
                                              "\ncaptureEnvLoop:"
                                              "beq $t0, $0, captureEnvReturn \t# if(env-counter == 0) return"
                                              "lw $t5, ($t3)"
                                              "sw $t5, ($t4) \t\t# copy one env value"
                                              "addi $t3, $t3, 4"
                                              "addi $t4, $t4, 4 \t# move both pointers one position"
                                              "sub $t0, $t0, 1 \t# decrease env-counter"
                                              "j captureEnvLoop"
                                              "\ncaptureEnvReturn:"
                                              "jr $ra"
                                              ))]
           [copy (inline (list "# t0: copy-count"
                               "# $t3: origin-pointer"
                               "# $t4: destination-pointer"
                               "# DESCRIPTION: copies the memory starting at $t3 to $t4 by a size of $t0 words"
                               "# USES: $t5"
                               "\ncopy:"
                               "beq $t0, $0, copyReturn # if(copy-counter == 0) return"
                               "lw $t5, ($t3)"
                               "sw $t5, ($t4) \t\t# copy one value"
                               "addi $t3, $t3, 4"
                               "addi $t4, $t4, 4 \t# move both pointers one position"
                               "sub $t0, $t0, 1 \t# decrease env-counter"
                               "j copy"
                               "\ncopyReturn:"
                               "jr $ra"
                               ""))]
           [access (inline (list "# $t0: counter"
                                 "# $t3: pointer to last structure"
                                 "# DESCRIPTION: leaves $t3 pointing to the structure at env[$t0]"
                                 "# USES: $t2"
                                 "\naccess:"
                                 "beq $t0, $0, accessReturn"
                                 "lw $t2, ($t3) \t\t# t2 = struct size"
                                 "mult $t2, $t1"
                                 "mflo $t2"
                                 "add $t3, $t3, $t2"
                                 "sub $t0, $t0, 1 \t\t# decrease the counter"
                                 "j access"
                                 "\naccessReturn:"
                                 "jr $ra"
                                 ))]
           [ending (inline (list "end:"
                                 "addiu $sp, $sp, 4 \t# move sp to value"
                                 "li\t$v0, 1 \t\t# code 1 for print integer"
                                 "lw\t$a0, 0($sp) \t# integer to print"
                                 "syscall"
                                 "li\t$v0, 4 \t\t# code 4 for print string"
                                 "la\t$a0, new_line \t# string to print"
                                 "syscall"
                                 "li\t$v0, 10 \t# code for exit"
                                 "syscall"
                                 ))]
           [comp (λ(e)(match e
                        [(INT_CONST n) (inline (list (string-append "# (INT_CONST " (~a n) ")")
                                                 "addi $sp, $sp, -4"
                                                 (string-append "lw $t0, int" (~a n))
                                                 "sw $t0, 0($sp)"
                                                 "addi $sp, $sp, -4"
                                                 "li $t0, 2"
                                                 "sw $t0, 0($sp) \t# put the size of int"
                                                 ))]
                        [(CLOSURE_CONST n) (inline (list (string-append "# (CLOSURE_CONST " (~a n) ")")
                                                         "addi $sp, $sp, -4"
                                                         "#sw $ra, 0($fp)"
                                                         
                                                         "move $t9, $ra"
                                                         "jal captureEnv"
                                                         "move $ra, $t9"
                                                         
                                                         "#lw $ra, 0($fp)"
                                                         (string-append "la $t0, " (~a n))
                                                         "sw $t0, 0($sp)"
                                                         "addi $sp, $sp, -4"
                                                         "lw $t0, 12($fp)"
                                                         "addiu $t0, $t0, 3"
                                                         "sw $t0, 0($sp) \t\t# put the function size at top"
                                                         ))]
                        [(ACCESS n) (inline (list (string-append "\n\t# (ACCESS " (~a n) ")")
                                                  "addi $t3, $fp, 16 \t# t3 points to first env value"
                                                  "li $t1, 4"
                                                  (string-append "li $t0," (~a (- n 1)) "\t\t# position counter")
                                                  
                                                  "move $t9, $ra"
                                                  "jal access"
                                                  "move $ra, $t9"
                                                  
                                                  "lw $t0, ($t3) \t\t# save the struct size"
                                                  "move $t2, $t0"
                                                  "mult $t2, $t1"
                                                  "mflo $t2"
                                                  "sub $t4, $sp, $t2"
                                                  "move $sp, $t4"
                                                  "move $t9, $ra"
                                                  "jal copy"
                                                  "move $ra, $t9"
                                                  ))]
                        [(ADD) (inline (list "# (ADD)"
                                             "lw $t0, 4($sp)"
                                             "lw $t1, 12($sp)"
                                             "add $t1, $t1, $t0"
                                             "sw $t1, 12($sp)"
                                             "addiu $sp, $sp, 8"
                                             ))]
                        [(SUB) (inline (list "# (SUB)"
                                             "lw $t0, 4($sp)"
                                             "lw $t1, 12($sp)"
                                             "sub $t1, $t1, $t0"
                                             "sw $t1, 12($sp)"
                                             "addiu $sp, $sp, 8"
                                             ))]
                        [(APPLY) (inline (list "# (APPLY)"
                                               "# copy the function environment into a new frame"
                                               "lw $t0, 0($sp) \t\t# argument-size into $t0"
                                               "li $t1, 4"
                                               "mult $t0, $t1"
                                               "mflo $t0 \t\t# t0 = t0 * 4 (alignment)"
                                               "add $t0, $sp $t0 \t# $t0 points to function size"
                                               
                                               "# calculate the $ra for the call and save it into t8"
                                               "# $ra is the address at the base of the calling function" 
                                               "lw $t8, ($t0)"
                                               "sub $t8, $t8, 1"
                                               "mult $t8, $t1"
                                               "mflo $t8 \t\t# t4 = t4 * 4 (alignment)"
                                               "add $t8, $t0, $t8 \t# return_address in $t8"
                                               
                                               "lw $t6, 4($t0) \t\t# store function pointer to the end"
                                               "add $t3, $t0, 12 \t# t3 points to env start"
                                               "lw $t4, 8($t0) \t\t# t4 = env-size"
                                               "move $t2, $t4 \t\t# t2 env-size"
                                               "mult $t4, $t1"
                                               "mflo $t4 \t\t# t4 = t4 * 4 (alignment)"
                                               "sub $t4, $sp, $t4"
                                               "move $t5, $t4"
                                               "move $t0, $t2"
                                               "move $t9, $ra"
                                               "jal copy"
                                               "move $ra, $t9"
                                               "# copy the argument into the head of the new frame environment"
                                               "lw $t0, 0($sp) \t\t# argument-size into $t0"
                                               "add $t2, $t2, $t0 \t# t2 = env-size + arg-size"
                                               "move $t4, $t2"
                                               "mult $t4, $t1"
                                               "mflo $t4 \t\t# t4 = t4 * 4 (alignment)"
                                               "sub $t4, $sp, $t4 \t# t4 aligned to copy arg"
                                               "move $t7, $t4"
                                               "# addi $t2, $t2, 1"
                                               "sw $t2, -4($t4) \t\t# new env-size positioned"
                                               "move $t3, $sp \t\t# t3 points to argument start"
                                               "move $t9, $ra"
                                               "jal copy"
                                               "move $ra, $t9"
                                               "# copy old frame values"
                                               "sw $ra, -8($t7)"
                                               "sw $fp, -12($t7)"
                                               "sw $t8, -16($t7)"
                                               "sub $t7, $t7, 16"
                                               "move $sp, $t7"
                                               "move $fp, $sp"
                                               "jal $t6 \t\t# call function"
                                               ))]
                        [(RETURN) (inline (list ""
                                                "# (RETURN)"
                                                "lw $t0, 0($sp) \t\t# return value-size into $t0"
                                                "lw $t8, 0($fp) \t\t# return address base into $t8"
                                                "sub $t4, $t0, 1"
                                                "li $t1, 4"
                                                "mult $t4, $t1"
                                                "mflo $t4 \t\t# t4 = t4 * 4 (alignment)"
                                                "sub $t4, $t8, $t4"
                                                "move $t3, $sp"
                                                "move $t6, $t4"
                                                "move $t9, $ra"
                                                "jal copy"
                                                "move $ra, $t9"
                                                "move $sp, $t6 \t# restore $sp"
                                                "lw $ra, 8($fp) \t# restore $ra"
                                                "lw $fp, 4($fp) \t# restore $fp"
                                                "jr $ra"))]))] 
           [funDefs (inline (hash-map replacedClosureHash
                                      (λ(k v) (string-append "\n" v ": # label for "
                                                             v
                                                             "\n\tsw $ra, 8($fp) \t# save old $ra"
                                                             (apply string-append (map comp (CLOSURE-ins k)))
                                                             ))
                                      
                                      ))])
    (string-append "\t\t.data\n"
                   "new_line:\t.asciiz \"\\n\"\n"
                   constants
                   "\n\t\t.text\n"
                   copy
                   access
                   captureEnvPrimitive
                   funDefs
                   "main:\n"
                   "\taddi $sp, $sp, -12\n"
                   "\tmove $fp, $sp\n"
                   (foldr (λ(x y) (string-append x "\n" y)) 
                          ""
                          (map comp (replaceClosures ins-list)))
                   ending
                   )))

(defun (spim-compile-to-file s-expr filename)
  (display-to-file (spim-compile (compile s-expr))
                   filename
                   #:mode 'text
                   #:exists 'replace))


#;(let ([prog '{{fun {x} {fun {y} y}} 0}])
    (display (spim-compile (compile prog)))
    (spim-compile-to-file prog "s.s"))

#;(let ([prog '{{{fun {x} {fun {y} x}} 3} 4}])
    (display (spim-compile (compile prog)))
    (spim-compile-to-file prog "s.s"))

(let ([prog '{{fun {f} {f 1}} {fun {x} {+ x 1}}}])
    (display (spim-compile (compile prog)))
    (spim-compile-to-file prog "s.s"))

#;(let ([prog '{+ {fun {f} f} 3}])
    (display (spim-compile (compile prog)))
    (spim-compile-to-file prog "s.s"))

#;(let ([prog '{{{fun {f} 
                      {fun {arg} {f arg}}} {fun {x} {+ x 1}}} 5}])
    (display (spim-compile (compile prog)))
    (spim-compile-to-file prog "s.s"))

#;(let ([prog  '{
               {{fun {f}
                     {{fun {frec} {frec frec}}
                      {fun {next}
                           {fun {n}
                                {{f {next next}} n}}}}}
                
                {fun {next}
                     {fun {n}
                          {next n}}}}
               5}])
  (spim-compile-to-file prog "s.s"))



