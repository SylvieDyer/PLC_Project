; If you are using scheme instead of racket, comment these two lines, uncomment the (load "simpleParser.scm") and comment the (require "simpleParser.rkt")
#lang racket
(require "functionParser.rkt")
(provide (all-defined-out))
; (load "simpleParser.scm")

; An interpreter for the simple language using tail recursion for the M_state functions and does not handle side effects.

; The functions that start interpret-...  all return the current environment.  These are the M_state functions.
; The functions that start eval-...  all return a value.  These are the M_value and M_boolean functions.

; The main function.  Calls parser to get the parse tree and interprets it with a new environment.  Sets default continuations for return, break, continue, throw, and "next statement"
(define interpret
  (lambda (file)
    (scheme->language
     (call/cc (lambda (k)
     (interpret-statement-list (parser file) (newenvironment) k
                               (lambda (env) (myerror "Break used outside of loop")) (lambda (env) (myerror "Continue used outside of loop"))
                               (lambda (v env) (myerror "Uncaught exception thrown")) (lambda (env) env))))) ))

; interprets a list of statements.  The state/environment from each statement is used for the next ones.
(define interpret-statement-list
  (lambda (statement-list environment return break continue throw next)
    (if (null? statement-list)
        (next environment)
        (interpret-statement (car statement-list) environment return break continue throw (lambda (env) (interpret-statement-list (cdr statement-list) env return break continue throw next))))))

; interpret a statement in the environment with continuations for return, break, continue, throw, and "next statement"
(define interpret-statement
  (lambda (statement environment return break continue throw next)
    (cond
      ; if at main function, want to run automatically
      ((eq? 'main (main-func? statement)) (interpret-statement-list (main-body statement) (push-frame environment) return break continue throw next))
      ((eq? 'function (statement-type statement)) (interpret-function (cdr statement) environment next))
      ((eq? 'funcall (statement-type statement))  (interpret-function-call (cdr statement) (push-frame environment) throw next #f))
      ((eq? 'return (statement-type statement))   (interpret-return statement environment return))
      ((eq? 'var (statement-type statement))      (interpret-declare statement environment next))
      ((eq? '= (statement-type statement))        (interpret-assign statement environment throw next))
      ((eq? 'if (statement-type statement))       (interpret-if statement environment return break continue throw next))
      ((eq? 'while (statement-type statement))    (interpret-while statement environment return throw next))
      ((eq? 'continue (statement-type statement)) (continue environment))
      ((eq? 'break (statement-type statement))    (break environment))
      ((eq? 'begin (statement-type statement))    (interpret-block statement environment return break continue throw next))
      ((eq? 'throw (statement-type statement))    (interpret-throw statement environment throw))
      ((eq? 'try (statement-type statement))      (interpret-try statement environment return break continue throw next))
      (else (myerror "Unknown statement:"         (statement-type statement))))
    ))

; Calls the function continuation
(define interpret-function
  (lambda (statement environment next)
    ; continue on, after binding the closure to the function's name 
    (next (insert (get-function-name statement) (make-closure (get-function-params statement) (get-function-body statement) environment) environment))))

; to handel when a function was called
(define interpret-function-call
  (lambda (statement environment throw next willReturn)
    ; check if the function has been declared
    (if (exists? (get-function-name statement) environment)
        ; get the closure
        (let ([closure (lookup (get-function-name statement) environment)])
          ; determine the return value of the function
          (let ([val (interpret-statement-list (get-closure-body closure)
                                               ; new state with formal/actual parameters added to the NEW state, with the closure in it
                                               (add-frame (bind-parameters (get-closure-params closure) (cdr statement) environment) (insert (get-function-name statement) closure (get-closure-state closure)))
                                               (lambda (v) v)
                                               (lambda (env) (myerror "Break used outside of loop")) (lambda (env) (myerror "Continue used outside of loop"))
                                               throw;(lambda (v env) (print "USEDDD????? :") (println environment) (throw v environment)) ;(lambda (v env) (myerror "Uncaught exception thrown"))
                                               (lambda (env)  env))]) ;env ???
            ; if main returns this function,
            (if willReturn
                ; return the value
                val
                ; otherwise pass the environment (which has been changed based on the function)
                (next environment))))
        ; if the function hasn't been declared 
        (myerror "Function undefined:" (get-function-name statement)))))
                
; Calls the return continuation with the given expression value
(define interpret-return
  (lambda (statement environment return)
    (return (eval-expression (get-expr statement) environment))))

; Adds a new variable binding to the environment.  There may be an assignment with the variable
(define interpret-declare
  (lambda (statement environment next)
    (if (exists-declare-value? statement)
        (next (insert (get-declare-var statement) (eval-expression (get-declare-value statement) environment) environment))
        (next (insert (get-declare-var statement) 'novalue environment)))))

; Updates the environment to add a new binding for a variable
(define interpret-assign
  (lambda (statement environment throw next)
    ; if there are function calls in the assign
    (if (< 0 (indexof 'funcall (get-assign-rhs statement)))
        ; run interpret-funccall w new throws, get value from that (willReturn True)
        (update (get-assign-lhs statement)
                (eval-expression (replaceIndexWith (indexof 'funcall (get-assign-rhs statement))
                                                   (get-assign-rhs statement)
                                                   (interpret-function-call (getAtIndex (indexof 'funcall (get-assign-rhs statement))
                                                                                        (get-assign-rhs statement))
                                                                            environment (lambda (v env) (throw v environment))
                                                                            next #t))
                                 environment)
                environment)
        ; otherwise, evaluate rhs as normal
        (update (get-assign-lhs statement) (eval-expression (get-assign-rhs statement) environment) environment))
    
    (next environment)))

; We need to check if there is an else condition.  Otherwise, we evaluate the expression and do the right thing.
(define interpret-if
  (lambda (statement environment return break continue throw next)
    (cond
      ((eval-expression (get-condition statement) environment) (interpret-statement (get-then statement) environment return break continue throw next))
      ((exists-else? statement) (interpret-statement (get-else statement) environment return break continue throw next))
      (else (next environment)))))

; Interprets a while loop.  We must create break and continue continuations for this loop
(define interpret-while
  (lambda (statement environment return throw next)
    (letrec ((loop (lambda (condition body environment)
                     (if (eval-expression condition environment)
                         (interpret-statement body environment return (lambda (env) (next env)) (lambda (env) (loop condition body env)) throw (lambda (env) (loop condition body env)))
                         (next environment)))))
      (loop (get-condition statement) (get-body statement) environment))))

; Interprets a block.  The break, continue, throw and "next statement" continuations must be adjusted to pop the environment
(define interpret-block
  (lambda (statement environment return break continue throw next)
    (interpret-statement-list (cdr statement)
                                         environment;(push-frame environment)
                                         return
                                         (lambda (env) (break (pop-frame env)))
                                         (lambda (env) (continue (pop-frame env)))
                                         throw
                                         ;;(lambda (v env) ;(print "THROW IN INTERPRET-BLOCK env: ") (println env)
                                           ;(println "LOOK AT ME USED")
                                           ;(throw v (pop-frame env)))
                                           ;(throw v environment))
                                         ;(lambda (env) (next env)))))
                                         next)))

; We use a continuation to throw the proper value.  Because we are not using boxes, the environment/state must be thrown as well so any environment changes will be kept
(define interpret-throw
  (lambda (statement environment throw)
    (throw (eval-expression (get-expr statement) environment) environment)))

; Interpret a try-catch-finally block

; Create a continuation for the throw. If there is no catch, it has to interpret the finally block, and once that completes throw the exception.
;   Otherwise, it interprets the catch block with the exception bound to the thrown value and interprets the finally block when the catch is done
(define create-throw-catch-continuation
  (lambda (catch-statement environment return break continue throw next finally-block)
    (cond
      ((null? catch-statement) (lambda (ex env) (interpret-block finally-block env return break continue throw (lambda (env2) (throw ex env2))))) 
      ((not (eq? 'catch (statement-type catch-statement))) (myerror "Incorrect catch statement"))
      (else (lambda (ex env)
                  (interpret-statement-list 
                       (get-body catch-statement) 
                       (insert (catch-var catch-statement) ex (push-frame env))
                       return
                       (lambda (env2) (break (pop-frame env2))) 
                       (lambda (env2) (continue (pop-frame env2))) 
                       throw;(lambda (v env2) (throw v (pop-frame env2))) 
                       (lambda (env2) (interpret-block finally-block env2 return break continue throw next)))))))); LOOOOK HERE

; To interpret a try block, we must adjust  the return, break, continue continuations to interpret the finally block if any of them are used.
;  We must create a new throw continuation and then interpret the try block with the new continuations followed by the finally block with the old continuations
(define interpret-try
  (lambda (statement environment return break continue throw next)
    (let* ((finally-block (make-finally-block (get-finally statement)))
           (try-block (make-try-block (get-try statement)))
           (new-return (lambda (v) (interpret-block finally-block environment return break continue throw (lambda (env2) (return v)))))
           (new-break (lambda (env) (interpret-block finally-block env return break continue throw (lambda (env2) (break env2)))))
           (new-continue (lambda (env) (interpret-block finally-block env return break continue throw (lambda (env2) (continue env2)))))
           (new-throw (create-throw-catch-continuation (get-catch statement) environment (lambda (v) (return v)) break continue throw next finally-block)))
      (interpret-block try-block environment new-return new-break new-continue new-throw (lambda (env) (interpret-block finally-block env (lambda (v) (return v)) break continue throw next))))))

; helper methods so that I can reuse the interpret-block method on the try and finally blocks
(define make-try-block
  (lambda (try-statement)
    (cons 'begin try-statement)))

(define make-finally-block
  (lambda (finally-statement)
    (cond
      ((null? finally-statement) '(begin))
      ((not (eq? (statement-type finally-statement) 'finally)) (myerror "Incorrectly formatted finally block"))
      (else (cons 'begin (cadr finally-statement))))))

; Evaluates all possible boolean and arithmetic expressions, including constants, variables, and function calls
(define eval-expression
  (lambda (expr environment)
    (cond
      ((number? expr) expr)
      ((eq? expr 'true) #t)
      ((eq? expr 'false) #f)
      ((not (list? expr)) (lookup expr environment))
      ((eq? (statement-type expr) 'funcall) (interpret-function-call (cdr expr) environment (lambda (v env) v)(lambda (v) v) #t))
      (else (eval-operator expr environment)))))

; Evaluate a binary (or unary) operator.  Although this is not dealing with side effects, I have the routine evaluate the left operand first and then
; pass the result to eval-binary-op2 to evaluate the right operand.  This forces the operands to be evaluated in the proper order in case you choose
; to add side effects to the interpreter
(define eval-operator
  (lambda (expr environment)
    (cond
      ((eq? '! (operator expr)) (not (eval-expression (operand1 expr) environment)))
      ((and (eq? '- (operator expr)) (= 2 (length expr))) (- (eval-expression (operand1 expr) environment)))
      (else (eval-binary-op2 expr (eval-expression (operand1 expr) environment) environment)))))

; Complete the evaluation of the binary operator by evaluating the second operand and performing the operation.
(define eval-binary-op2
  (lambda (expr op1value environment)
    (cond
      ((eq? '+ (operator expr)) (+ op1value (eval-expression (operand2 expr) environment)))
      ((eq? '- (operator expr)) (- op1value (eval-expression (operand2 expr) environment)))
      ((eq? '* (operator expr)) (* op1value (eval-expression (operand2 expr) environment)))
      ((eq? '/ (operator expr)) (quotient op1value (eval-expression (operand2 expr) environment)))
      ((eq? '% (operator expr)) (remainder op1value (eval-expression (operand2 expr) environment)))
      ((eq? '== (operator expr)) (isequal op1value (eval-expression (operand2 expr) environment)))
      ((eq? '!= (operator expr)) (not (isequal op1value (eval-expression (operand2 expr) environment))))
      ((eq? '< (operator expr)) (< op1value (eval-expression (operand2 expr) environment)))
      ((eq? '> (operator expr)) (> op1value (eval-expression (operand2 expr) environment)))
      ((eq? '<= (operator expr)) (<= op1value (eval-expression (operand2 expr) environment)))
      ((eq? '>= (operator expr)) (>= op1value (eval-expression (operand2 expr) environment)))
      ((eq? '|| (operator expr)) (or op1value (eval-expression (operand2 expr) environment)))
      ((eq? '&& (operator expr)) (and op1value (eval-expression (operand2 expr) environment)))
      (else (println expr)(myerror "Unknown operator:" (operator expr))))))

; Determines if two values are equal.  We need a special test because there are both boolean and integer types.
(define isequal
  (lambda (val1 val2)
    (if (and (number? val1) (number? val2))
        (= val1 val2)
        (eq? val1 val2))))


;-----------------
; HELPER FUNCTIONS
;-----------------

; These helper functions define the operator and operands of a value expression
(define operator car)
(define operand1 cadr)
(define operand2 caddr)
(define operand3 cadddr)
(define main-body cadddr)

(define exists-operand2?
  (lambda (statement)
    (not (null? (cddr statement)))))

(define exists-operand3?
  (lambda (statement)
    (not (null? (cdddr statement)))))

; these helper functions define the parts of the various statement types
(define statement-type operator)
(define get-expr operand1)
(define get-declare-var operand1)
(define get-declare-value operand2)
(define exists-declare-value? exists-operand2?)
(define get-assign-lhs operand1)
(define get-assign-rhs operand2)
(define get-condition operand1)
(define get-then operand2)
(define get-else operand3)
(define get-body operand2)
(define exists-else? exists-operand3?)
(define get-try operand1)
(define get-catch operand2)
(define get-finally operand3)

(define main-func? operand1)

(define get-function-name operator)
(define get-function-params operand1)
(define get-function-body operand2)

(define get-closure-params operator)
(define get-closure-body operand1)
(define get-closure-state operand2)

(define catch-var
  (lambda (catch-statement)
    (car (operand1 catch-statement))))


;------------------------
; Environment/State Functions
;------------------------

; create a new empty environment
(define newenvironment
  (lambda ()
    (list (newframe))))

; create an empty frame: a frame is two lists, the first are the variables and the second is the "store" of values
(define newframe
  (lambda ()
    '(() ())))

; add a frame onto the top of the environment
(define push-frame
  (lambda (environment)
    (cons (newframe) environment)))

; remove a frame from the environment
(define pop-frame
  (lambda (environment)
    (cdr environment)))

; add a frame onto the top of the environment
(define add-frame
  (lambda (new-frame environment)
    (cons (topframe new-frame) environment)))

; some abstractions
(define topframe car)
(define remainingframes cdr)

; does a variable exist in the environment?
(define exists?
  (lambda (var environment)
    (cond
      ((null? environment) #f)
      ((exists-in-list? var (variables (topframe environment))) #t)
      (else (exists? var (remainingframes environment))))))

; does a variable exist in a list?
(define exists-in-list?
  (lambda (var l)
    (cond
      ((null? l) #f)
      ((eq? var (car l)) #t)
      (else (exists-in-list? var (cdr l))))))

; Looks up a value in the environment.  If the value is a boolean, it converts our languages boolean type to a Scheme boolean type
(define lookup
  (lambda (var environment)
    (lookup-variable var environment)))
  
; A helper function that does the lookup.  Returns an error if the variable does not have a legal value
(define lookup-variable
  (lambda (var environment)
    (let ((value (lookup-in-env var environment))) 
      (if (eq? 'novalue value)
          (myerror "error: variable without an assigned value:" var)
          value)))) 

; Return the value bound to a variable in the environment
(define lookup-in-env
  (lambda (var environment)
    (cond
      ((null? environment) (myerror "error: undefined variable" var))
      ((exists-in-list? var (variables (topframe environment))) (lookup-in-frame var (topframe environment)))
      (else (lookup-in-env var (cdr environment))))))

; Return the value bound to a variable in the frame
(define lookup-in-frame
  (lambda (var frame)
    (cond
      ((not (exists-in-list? var (variables frame))) (myerror "error: undefined variable" var))
      (else (language->scheme (get-value (indexof var (variables frame)) (store frame)))))))

; Get the location of a name in a list of names
(define indexof
  (lambda (var l)
    (cond
      ((or (null? l) (not (list? l))) -100)  ; should not happen
      ((and (list? (car l)) (eq? var (caar l))) 0)
      ((eq? var (car l)) 0)
      (else (+ 1 (indexof var (cdr l)))))))

; get whatever is at that index
(define getAtIndex
  (lambda (index l)
    (getAtIndex-helper index 0 l)));(lambda (v) v))))

(define getAtIndex-helper
  (lambda (index currIndex l)
    (if (eq? index currIndex)
        (cdar l)
        (getAtIndex-helper index (+ currIndex 1) (cdr l)))))

; at index replace current val of l with new val
(define replaceIndexWith
  (lambda (index l newVal)
    (replaceIndexWith-helper index 0 l newVal (lambda (v) v))))

(define replaceIndexWith-helper
  (lambda (index currIndex l newVal return)
    (if (eq? index currIndex)
        (return (cons newVal  (cdr l)))
        (replaceIndexWith-helper index (+ currIndex 1) (cdr l) newVal (lambda (v) (return (cons (car l) v)))))))

; Get the value stored at a given index in the list
(define get-value
  (lambda (n l)
    (cond
      ((zero? n) (unbox (car l))) ; UNBOX THE CAR HERE
      (else (get-value (- n 1) (cdr l))))))

; Adds a new variable/value binding pair into the environment.  Gives an error if the variable already exists in this frame.
(define insert
  (lambda (var val environment)
    (if (exists-in-list? var (variables (car environment)))
        (myerror "error: variable is being re-declared:" var)
        (cons (add-to-frame var val (car environment)) (cdr environment)))))

; Changes the binding of a variable to a new value in the environment.  Gives an error if the variable does not exist.
(define update
  (lambda (var val environment)
    (if (exists? var environment)
        (update-existing var val environment)
        (myerror "error: variable used but not defined:" var))))

; Add a new variable/value pair to the frame.
(define add-to-frame
  (lambda (var val frame)
    (list (cons var (variables frame)) (cons (box (scheme->language val)) (store frame))))) ; BOX scheme->language result

; Changes the binding of a variable in the environment to a new value
(define update-existing
  (lambda (var val environment)
    (if (exists-in-list? var (variables (car environment)))
        (update-in-frame var val (topframe environment))
        (update-existing var val (remainingframes environment)))))
       ; (cons (update-in-frame var val (topframe environment)) (remainingframes environment))
        ;(cons (topframe environment) (update-existing var val (remainingframes environment))))))

; Changes the binding of a variable in the frame to a new value.
(define update-in-frame
  (lambda (var val frame)
    ;(list (variables frame) (update-in-frame-store var val (variables frame) (store frame)))
     (update-in-frame-store var val (variables frame) (store frame))))

; Changes a variable binding by placing the new value in the appropriate place in the store
(define update-in-frame-store
  (lambda (var val varlist vallist)
    (cond
      ((eq? var (car varlist))  (set-box! (car vallist) (scheme->language val)));(cons (scheme->language val) (cdr vallist))) ; SETBOX
      (else (update-in-frame-store var val (cdr varlist) (cdr vallist))))));(cons (car vallist) (update-in-frame-store var val (cdr varlist) (cdr vallist)))))))

; Returns the list of variables from a frame
(define variables
  (lambda (frame)
    (car frame)))

; Returns the store from a frame
(define store
  (lambda (frame)
    (cadr frame)))


; to make the closure
(define make-closure
  (lambda (formal-params body environment)
    (cons formal-params (cons body (cons environment '())))))

; binding formal and actual parameters into a frame and returning that frame
(define bind-parameters
  (lambda (formal actual environment)
    ; if there are the same number of formal and actual parameters
    (if (eq? (length formal) (length actual))
        ; bind the parameters 
        (bind-parameters-helper formal actual (newenvironment) environment)
        ; otherwise, error
        (myerror "Mismatched parameters and arguments." ))))

(define bind-parameters-helper
  (lambda (formal actual frame environment)
    (cond
      ((null? formal)   frame)
      (else            (bind-parameters-helper (cdr formal) (cdr actual) (insert (operator formal) (eval-expression (operator actual) environment) frame) environment) ))))



; Functions to convert the Scheme #t and #f to our languages true and false, and back.
(define language->scheme
  (lambda (v) 
    (cond 
      ((eq? v 'false) #f)
      ((eq? v 'true) #t)
      (else v))))

(define scheme->language
  (lambda (v)
    (cond
      ((eq? v #f) 'false)
      ((eq? v #t) 'true)
      (else v))))

; Because the error function is not defined in R5RS scheme, I create my own:
(define error-break (lambda (v) v))
(call-with-current-continuation (lambda (k) (set! error-break k)))

(define myerror
  (lambda (str . vals)
    (letrec ((makestr (lambda (str vals)
                        (if (null? vals)
                            str
                            (makestr (string-append str (string-append " " (symbol->string (car vals)))) (cdr vals))))))
      (error-break (display (string-append str (makestr "" vals)))))))

