#lang play

(require "stack.rkt")

(print-only-errors #t)

;;;;;;;;;;;;;;;;;;;;;;;
;; Machine definition
;;;;;;;;;;;;;;;;;;;;;;;

(deftype Instruction
  (CONST n)
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

(test/exn (machine (list
                    (CLOSURE (list (ACCESS 1) 
                                   (ACCESS 1) 
                                   (CONST 1) 
                                   (APPLY) 
                                   (APPLY) 
                                   (RETURN)))
                    (LET)
                    (ACCESS 1)
                    (ACCESS 1)
                    (CONST 1)
                    (APPLY)
                    (APPLY)
                    (ENDLET))) "SIGFAULT")

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

(test (compile '{with {fun {f} {f {f 1}}} in
                      {{acc 1} {{acc 1} 1}}})
      (list
       (CLOSURE (list (ACCESS 1) 
                      (ACCESS 1) 
                      (CONST 1) 
                      (APPLY) 
                      (APPLY) 
                      (RETURN)))
       (LET)
       (ACCESS 1)
       (ACCESS 1)
       (CONST 1)
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


(test (collectClosures (list (CLOSURE (list (CLOSURE (list (CONST 1) 
                                                           (CONST 2) 
                                                           (ADD) 
                                                           (RETURN))) 
                                            (RETURN))) 
                             (CONST 3) 
                             (APPLY)))
      (list (CLOSURE (list (CLOSURE (list (CONST 1) 
                                          (CONST 2) 
                                          (ADD) 
                                          (RETURN))) 
                           (RETURN)))
            (CLOSURE (list (CONST 1) 
                           (CONST 2) 
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

;; $t8 -> environment length (each time a function is called is incremented by one)

;; 0($fp) -> return address
;; 4($fp) -> old frame pointer 
;; 8($fp) -> env[0] (new-arg)
;; 12($fp) -> env[1]

;; $t9  -> old stack pointer  ($sp)
;; $t10 -> old frame pointer  ($fp)
;; $t11 -> old return address ($ra)

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
                             (map (λ(c) (let ([num (~a (CONST-n c))])
                                          (string-append "int" num ":\t.word\t" num "\n")))
                                  (remove-duplicates (append (filter (λ(e)(CONST? e)) ins-list)
                                                             (apply append (hash-map replacedClosureHash
                                                                                     (λ(k v)
                                                                                       (match k
                                                                                         [(CLOSURE ins) (filter CONST? ins)]))))))))]
           [copyEnvPrimivite (inline (list "\ncopyEnvReturn:"
                                           "# lw $t0, 0($fp)"
                                           "jr $ra"
                                           ""
                                           "\ncopyEnv: \t\t\t# after this call a zero must appear at 0($sp)"
                                           "beq $t8, $0, copyEnvReturn \t# if env.size == 0 return"
                                           "lw $t0, 0($sp)"
                                           "addi $t3, $t0, 3 \t# t3 = t0 + 3 (for the old_fp and old_sp)"
                                           "li $t1, 4"
                                           "mult $t3,$t1"
                                           "mflo $t3 \t\t# t3 = t3 * 4 (alignment)"
                                           "add $t4, $t3, $fp"
                                           "# sw ($t4), 0($sp) \t# sp[0] = fp[t3] ($t4 is positioned at env[arg])"
                                           "sub $t5, $t8, $t0 \t# t5 = env-size - arg"
                                           "mult $t5,$t1"
                                           "mflo $t5 \t\t# t5 = t5 * 4 (alignment)"
                                           "sub $t5, $sp, $t5"
                                           "lw $t4, ($t4)"
                                           "sw $t4, ($t5) \t\t# the actual copy"
                                           "# addi $sp, $sp, -4 \t# (reposition the stack pointer)"
                                           "beq $t0, $0, copyEnvReturn\t# if(t0 == 0) return"
                                           "addi $t3, $t0, -1 \t# t3 = t0 - 1"
                                           "sw $t3, 0($sp) \t\t# sp[0] = t3"
                                           "j copyEnv"
                                           ))]
           [captureEnvPrimitive (inline (list "\ncaptureEnv:"
                                              "lw $t0, 12($fp) \t\t# t0 = env-size"
                                              "li $t1, 4"
                                              "move $t3, $t0"
                                              "mult $t3, $t1"
                                              "mflo $t3 \t\t# t3 = t3 * 4 (alignment)"
                                              "sub $t3, $sp, $t3 \t# t3 is the position for the env-size"
                                              "sw $t0, ($t3)"
                                              "move $sp, $t3"
                                              "addi $sp, $sp, -4 \t# position sp to receive function pointer"
                                              "addi $t4, $t3, 4 \t# position t4 to receive the copy"
                                              "addi $t3, $fp, 16 \t# position t3 to source the copy"
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
           [ending (inline (list "li\t$v0, 1 \t\t# code 1 for print integer"
                                 "lw\t$a0, 0($sp) \t# integer to print"
                                 "syscall"
                                 "li\t$v0, 4 \t\t# code 4 for print string"
                                 "la\t$a0, new_line \t# string to print"
                                 "syscall"
                                 "li\t$v0, 10 \t# code for exit"
                                 "syscall"
                                 ))]
           [comp (λ(e)(match e
                        [(CONST n) (inline (list (string-append "# (CONST " (~a n) ")")
                                                 "addi $sp, $sp, -4"
                                                 (string-append "lw $t0, int" (~a n))
                                                 "sw $t0, 0($sp)"
                                                 ))]
                        [(CLOSURE_CONST n) (inline (list (string-append "# (CLOSURE_CONST " (~a n) ")")
                                                         "addi $sp, $sp, -4"
                                                         "sw $ra, 0($fp)"
                                                         "jal captureEnv"
                                                         "lw $ra, 0($fp)"
                                                         (string-append "la $t0, " (~a n))
                                                         "sw $t0, 0($sp)"
                                                         ))]
                        [(ACCESS n) (inline (list (string-append "\n\t# (ACCESS " (~a n) ")")
                                                  "addi $sp, $sp, -4"
                                                  "li $t1, 2"
                                                  (string-append "addi $t0, $t1, " (~a n))
                                                  "li $t1, 4"
                                                  "mult $t0, $t1"
                                                  "mflo $t0 		# t3 = t3 * 4 (alignment)"
                                                  "add $t0, $fp, $t0"
                                                  "lw $t1, ($t0)"
                                                  "sw $t1, 0($sp)"))]
                        [(ADD) (inline (list "# (ADD)"
                                             "lw $t0, 0($sp)"
                                             "addi $sp, $sp, 4"
                                             "lw $t1, 0($sp)"
                                             "add $t1, $t1, $t0"
                                             "sw $t1, 0($sp)"
                                             ))]
                        [(SUB) (inline (list "# (SUB)"
                                             "lw $t0, 0($sp)"
                                             "addi $sp, $sp, 4"
                                             "lw $t1, 0($sp)"
                                             "sub $t1, $t1, $t0"
                                             "sw $t1, 0($sp)"
                                             ))]
                        [(APPLY) (inline (list "# (APPLY)"
                                               "lw $t0, 0($sp) \t\t# argument into $t0"
                                               "addi $t3,$t8,2 \t\t# t3 = env-size + 2"
                                               "li $t1, 4"
                                               "mult $t3, $t1"
                                               "mflo $t3 \t\t# t3 = t3 * 4 (alignment)"
                                               "sub $t3, $sp, $t3"
                                               "sw $t0, ($t3) \t\t# sp[t3] = t0"
                                               "addi $sp, $sp, -4"
                                               "lw $t8, 0($sp) \t\t# argument for copyEnv"
                                               "jal copyEnv"
                                               "addi $t3, $t8, 2"
                                               "li $t1, 4"
                                               "mult $t3, $t1"
                                               "mflo $t3 \t\t# t3 = t3 * 4 (alignment)"
                                               "sub $t3, $sp, $t3"
                                               "addi $sp, $sp, 8"
                                               "sw $sp, ($t3) \t\t# save old stack pointer"
                                               "addi $t3, $t3, -4"
                                               "sw $fp, ($t3) \t\t# save old frame pointer"
                                               "addi $t3, $t3, -4"
                                               "sw $ra, ($t3) \t\t# save return address"
                                               "lw $t1, 0($sp) \t\t# function location into $t1"
                                               "move $sp, $t3 \t\t# move the stack to the new stack position"
                                               "move $fp, $sp \t\t# refresh frame pointer (below stack)"
                                               "addiu $t3, $t3, -4 \t# position t3 at new stack position"
                                               "jal $t1 \t\t# call function"))]
                        [(RETURN) (inline (list ""
                                                "# (RETURN)"
                                                "lw $t0, 0($sp) \t\t# return value into $t0"
                                                "lw $t7, 0($fp) \t\t# restore old return address to t7"
                                                "lw $t6, 4($fp) \t\t# restore old frame pointer to t6"
                                                "lw $t5, 8($fp) \t\t# restore old stack pointer to t5"
                                                "move $sp, $t5"
                                                "move $fp, $t6"
                                                "move $ra, $t7"
                                                "sw $t0, 0($sp) \t\t# place return var into stack"
                                                "addi $t8, $t8, -1 \t# env-size - 1"
                                                "jr	$ra"))]))] 
           [funDefs (inline (hash-map replacedClosureHash
                                      (λ(k v) (string-append "\n" v ": # label for "
                                                             v
                                                             "\n\t" 
                                                             "addi $t8, $t8, 1 \t# env-size + 1\n"
                                                             "\tsw $ra, 0($fp)\n"
                                                             (apply string-append (map comp (CLOSURE-ins k)))
                                                             ))
                                      
                                      ))])
    (string-append "\t\t.data\n"
                   "new_line:\t.asciiz \"\\n\"\n"
                   constants
                   "\n\t\t.text\n"
                   captureEnvPrimitive
                   copyEnvPrimivite
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


(let ([prog '{fun {x} 1}])
  (display (spim-compile (compile prog)))
  (spim-compile-to-file prog "s.s"))

