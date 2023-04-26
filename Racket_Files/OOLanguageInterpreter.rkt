; Imperitive Language Interpreter Project
; Luis Torres & Sylvie Dyer
; Based on Professor Connamacher's Language Interpreter, CPS Version
#lang racket
(require "classParser.rkt")

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
                               (lambda (v env) (myerror "Uncaught exception thrown")) (lambda (env) (println "return") (println env) env))))) ))

; interprets a list of statements.  The state/environment from each statement is used for the next ones.
(define interpret-statement-list
  (lambda (statement-list environment currType instance return break continue throw next)
    (if (null? statement-list)
        (next environment)
        (interpret-statement (car statement-list) environment currType instance return break continue throw (lambda (env) (interpret-statement-list (cdr statement-list) env currType instance return break continue throw next))))))

; interpret a statement in the environment with continuations for return, break, continue, throw, and "next statement"
(define interpret-statement
  (lambda (statement environment currType instance return break continue throw next)
    (cond
      ; if at main function, want to run automatically
      ((eq? 'main (main-func? statement)) (interpret-statement-list (main-body statement) (push-frame environment) currType instance return break continue throw next))
     ; ((eq? 'class (statement-type statement))    (interpret-class (cdr statement) environment next))
     ;? don't need anymore? ((eq? 'function (statement-type statement)) (interpret-function (cdr statement) currType environment next))
      ((eq? 'funcall (statement-type statement))  (interpret-function-call (cdr statement) (push-frame environment) currType instance throw next #f))
      ((eq? 'return (statement-type statement))   (interpret-return statement environment currType instance return))
      ((eq? 'var (statement-type statement))      (interpret-declare statement environment currType instance next))
      ((eq? '= (statement-type statement))        (interpret-assign statement environment currType instance throw next))
      ((eq? 'if (statement-type statement))       (interpret-if statement environment currType instance return break continue throw next))
      ((eq? 'while (statement-type statement))    (interpret-while statement environment currType instance return throw next))
      ((eq? 'continue (statement-type statement)) (continue environment))
      ((eq? 'break (statement-type statement))    (break environment))
      ((eq? 'begin (statement-type statement))    (interpret-block statement environment currType instance return break continue throw next))
      ((eq? 'throw (statement-type statement))    (interpret-throw statement environment currType instance throw))
      ((eq? 'try (statement-type statement))      (interpret-try statement environment currType instance return break continue throw next))
      (else (myerror "Unknown statement:"         (statement-type statement))))
    ))

; -------------------- NEW TOP LEVEL STUFF FOR THE GLOBAL ENV STORING CLASSES ------------
; WILL WANT OT RENAME OR DELETE BUT I WAS TOO SCARED SO WE'LL DEAL
(define NEWINTERPRET
  (lambda (file classname)
    (scheme->language
              (INTERPRET-LIST (parser file) (newenvironment) (lambda (globalEnv)
                                                               ;(println "global env")(println globalEnv)
                                                               (call-main classname globalEnv))))))

(define INTERPRET-LIST
  (lambda (class-list globalEnvironment next)
    (if (null? class-list)
        (next globalEnvironment)
        (interpret-classes (car class-list) globalEnvironment (lambda (newEnv) (INTERPRET-LIST (cdr class-list) newEnv next))))))

; each statement in the list is a class (not the conditional as in interpret-statement)
(define interpret-classes
  (lambda (class globalEnvironment next)
    (interpret-class (cdr class) globalEnvironment next)))

; Recursively builds the elements of the class' closure 
(define build-class-closure
  (lambda (classDefinitionList super fieldsList methodsList classname classNext)
    (if (null? classDefinitionList)
        (classNext super fieldsList methodsList)
        (add-to-class-closure (car classDefinitionList) super fieldsList methodsList classname (lambda (s f m) (build-class-closure (cdr classDefinitionList) s f m classname classNext))))))

; building the class closure ^ called by build (maybe will be the same as interpret statement TODO:
(define add-to-class-closure
  (lambda (statement super fieldsList methodsList classname classNext)
    (cond
      ((eq? 'var (statement-type statement))                    (class-declare statement super fieldsList methodsList classname classNext))
      ((eq? 'static-function (statement-type statement))        (interpret-function (cdr statement) super fieldsList methodsList classNext #t))
      ; the below will need to add "this" to the parameter list (because it is not static!
      ((eq? 'function (statement-type statement))               (interpret-function (cdr statement) super fieldsList methodsList classNext #f))
      )))


; To build the class closure
(define interpret-class
  (lambda (classDefinition environment next)
    ; if there is a super class
    (if (extends? classDefinition)
        (build-class-closure (class-dec classDefinition)
                             (super-name classDefinition)
                             (newenvironment)
                             (newenvironment)
                             (class-name classDefinition)
                             (lambda (s f m)
                               (make-class-closure (class-name classDefinition) s f m environment next)))
                                                                                                                         
        ; otherwise, use this class for the parent [TODO: MAY BE SOMETHING ELSE FOR NAME]
        (build-class-closure (class-dec classDefinition)
                             (class-name classDefinition)
                             (newenvironment)
                             (newenvironment)
                             (class-name classDefinition)
                             (lambda (s f m)
                               (make-class-closure (class-name classDefinition) s f m environment next))))))

; takes the elements and makes the closure
(define make-class-closure
  (lambda (className superClass fieldsList methodsList globalEnvironment next)
    (next (insert className (cons superClass (cons fieldsList (cons methodsList '()))) globalEnvironment))))

; handling new, creating an instance 
(define instantiate
  (lambda (classname environment)
    ;(print "instantiating")
    ;(println classname)
    
    ; runtime type: classname
    ; values fo reach instance field (TODO: not initialized): get-closure-fieldList (lookup classname environment)
    (cons classname (cons (get-closure-fieldsList (lookup classname environment)) '()))))
  

; Adds a new variable binding to the environment.  There may be an assignment with the variable
(define class-declare
  (lambda (statement super fieldsList methodsList classname classNext)
    (if (exists-declare-value? statement)
        (classNext super (insert (get-declare-var statement) (eval-expression (get-declare-value statement) classname '() fieldsList) fieldsList) methodsList)
        (classNext super (insert (get-declare-var statement) 'novalue fieldsList) methodsList))))

; assigns a field name to an expression to compute its value
; THINK THIS WAS WASTE OF TIME, ISNT ACTUALLY CALLED RN
(define class-assign
  (lambda (statement super fieldsList methodsList classNext)
    ; match instance field name with expression to compute initial value
    (update (get-assign-lhs statement) (get-assign-rhs statement) fieldsList)
    (classNext super fieldsList methodsList)))


; Calls the function continuation
(define interpret-function
  (lambda (statement super fieldsList methodsList classNext isStatic)
    ; continue on, after binding the closure to the function's name
    (if isStatic
        ; if static, "this" is added to the function's parameter list 
        (classNext super fieldsList (insert (get-function-name statement) (make-closure (get-function-params statement) (get-function-body statement) methodsList super) methodsList))
        (classNext super fieldsList (insert (get-function-name statement) (make-closure (cons 'this (get-function-params statement)) (get-function-body statement) methodsList super) methodsList)))))

; to seach for a function and return its closure if it exists
(define function-search
  (lambda (closure funcName environment)
    (println (get-closure-methodsList closure))
    (println (exists-in-list? funcName  (car (get-closure-methodsList closure))))
    (cond
      ((null? closure)                                                 (myerror "Function undefined: " funcName))
      ((exists-in-list? funcName (car (get-closure-methodsList closure)) )   (lookup-in-frame funcName (get-closure-methodsList closure)))
      (else                                                            (function-search (lookup (get-closure-super closure) environment) funcName environment)))))

  
; to handle when a function was called
(define interpret-function-call
  (lambda (statement environment currType instance throw next willReturn)
    ; FOR ABSTRACTIONS LATER TODO:
    ; statement: '((dot instanceName functionName) params)
    ; instanceName: (cadr (car statement))
    ; functionName: (caddr (car statement))
    (println statement)
   (let ([classClosure (lookup (class-name (dot (cadr (car statement)) environment)) environment)])
     ;  the closure of the class this method is stored in (class-name instance)
     (let ([funcClosure (function-search classClosure (caddr (car statement)) environment)])
       ;(println (caddr (car statement)))
       ;(println funcClosure)
       ;(println (get-closure-params funcClosure))
      (let ([val (interpret-statement-list (get-closure-body funcClosure)
                                           ; new state with formal / actual parameters added to NEW state, with the closure in it
                                           (add-frame (bind-parameters (get-closure-params funcClosure) (cons (cadr (car statement)) (cdr statement)) environment currType instance) (insert (caddr (car statement)) funcClosure (get-closure-state funcClosure)))
                                           currType instance
                                           (lambda (v) v) (lambda (env) (myerror "Break used outside of loop")) (lambda (env) (myerror"Continue used outside of loop"))
                                           (lambda (v env) (throw v environment)) (lambda (env) env))])
        (if willReturn
            val
            (next (pop-frame environment))))))))

; to call the specified class's main method at the end of interpreting
(define call-main
  (lambda (classname globalEnv)
    ; the closure of the given class : lookup classname globalEnv
    ; the methods list : get-closure-methodsList (lookup clasname globalEnv)
    ; the main method closure : lookup 'main ^^
    (let ([main-closure (lookup-in-frame 'main (get-closure-methodsList (lookup classname globalEnv)))])
      (interpret-statement-list (get-closure-body main-closure)
                                ;not sure which env this is, but all i know is there are no parameters to bind so we add a frame (also wont recursively call main i dont think )
                                (add-frame (newenvironment) globalEnv)
                                classname (instantiate classname globalEnv)
                                (lambda (v) v) (lambda (env) (myerror "Break used outside of loop")) (lambda (env) (myerror "Continue used outside of loop")) (lambda (v env) (myerror "Uncaught exception thrown")) (lambda (env) env)))))

; dot operator to return the closure of the instance next to a dot
(define dot
  (lambda (instanceName environment)
    (if (exists? instanceName environment)
        (lookup instanceName environment)
        (myerror "Instance does not exist: " instanceName))))

; ---- end of the suff used for classes lmao 

; Calls the return continuation with the given expression value
(define interpret-return
  (lambda (statement environment currType instance return)
    (return (eval-expression (get-expr statement) environment currType instance))))

; Adds a new variable binding to the environment.  There may be an assignment with the variable
(define interpret-declare
  (lambda (statement environment currType instance next)
    (if (exists-declare-value? statement)
        (next (insert (get-declare-var statement) (eval-expression (get-declare-value statement) environment currType instance) environment))
        (next (insert (get-declare-var statement) 'novalue environment)))))

; Updates the environment to add a new binding for a variable
(define interpret-assign
  (lambda (statement environment currType instance throw next)
    (updateStatementWithFunctions (get-assign-rhs statement) environment throw next '() (lambda (s) (update (get-assign-lhs statement) (eval-expression s environment currType instance) environment)))
    (next environment) ));(update (get-assign-lhs statement) (eval-expression (get-assign-rhs statement) environment) environment))))

; We need to check if there is an else condition.  Otherwise, we evaluate the expression and do the right thing.
(define interpret-if
  (lambda (statement environment currType instance return break continue throw next)
    (cond
      ((eval-expression (get-condition statement) environment) (interpret-statement (get-then statement) environment return break continue throw next))
      ((exists-else? statement) (interpret-statement (get-else statement) environment return break continue throw next))
      (else (next environment)))))

; Interprets a while loop.  We must create break and continue continuations for this loop
(define interpret-while
  (lambda (statement environment currType instance return throw next)
    (letrec ((loop (lambda (condition body environment)
                     (if (eval-expression condition environment)
                         (interpret-statement body environment return (lambda (env) (next env)) (lambda (env) (loop condition body env)) throw (lambda (env) (loop condition body env)))
                         (next environment)))))
      (loop (get-condition statement) (get-body statement) environment))))

; Interprets a block.  The break, continue, throw and "next statement" continuations must be adjusted to pop the environment
(define interpret-block
  (lambda (statement environment currType instance return break continue throw next)
    (interpret-statement-list (cdr statement)
                                         (push-frame environment)
                                         return
                                         (lambda (env) (break (pop-frame env)))
                                         (lambda (env) (continue (pop-frame env)))
                                         (lambda (v env) (throw v (pop-frame env)))
                                         (lambda (env) (next (pop-frame env))))))

; We use a continuation to throw the proper value.  Because we are not using boxes, the environment/state must be thrown as well so any environment changes will be kept
(define interpret-throw
  (lambda (statement environment currType instance throw)
    (throw (eval-expression (get-expr statement) environment) environment)))

; Interpret a try-catch-finally block

; Create a continuation for the throw.  If there is no catch, it has to interpret the finally block, and once that completes throw the exception.
;   Otherwise, it interprets the catch block with the exception bound to the thrown value and interprets the finally block when the catch is done
(define create-throw-catch-continuation
  (lambda (catch-statement environment currType instance return break continue throw next finally-block)
    (cond
      ((null? catch-statement) (lambda (ex env) (interpret-block finally-block env currType instance return break continue throw (lambda (env2) (throw ex env2))))) 
      ((not (eq? 'catch (statement-type catch-statement))) (myerror "Incorrect catch statement"))
      (else (lambda (ex env)
                  (interpret-statement-list 
                       (get-body catch-statement) 
                       (insert (catch-var catch-statement) ex (push-frame env))
                       return 
                       (lambda (env2) (break (pop-frame env2))) 
                       (lambda (env2) (continue (pop-frame env2))) 
                       (lambda (v env2) (throw v (pop-frame env2))) 
                       (lambda (env2) (interpret-block finally-block (pop-frame env2) currType instance return break continue throw next))))))))

; To interpret a try block, we must adjust  the return, break, continue continuations to interpret the finally block if any of them are used.
;  We must create a new throw continuation and then interpret the try block with the new continuations followed by the finally block with the old continuations
(define interpret-try
  (lambda (statement environment currType instance return break continue throw next)
    (let* ((finally-block (make-finally-block (get-finally statement)))
           (try-block (make-try-block (get-try statement)))
           (new-return (lambda (v) (interpret-block finally-block environment currType instance return break continue throw (lambda (env2) (return v)))))
           (new-break (lambda (env) (interpret-block finally-block env currType instance return break continue throw (lambda (env2) (break env2)))))
           (new-continue (lambda (env) (interpret-block finally-block env currType instance return break continue throw (lambda (env2) (continue env2)))))
           (new-throw (create-throw-catch-continuation (get-catch statement) environment currType instance return break continue throw next finally-block)))
      (interpret-block try-block environment currType instance new-return new-break new-continue new-throw (lambda (env) (interpret-block finally-block env currType instance return break continue throw next))))))

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
  (lambda (expr environment currType instance)
    (cond
      ((number? expr) expr)
      ((eq? expr 'true) #t)
      ((eq? expr 'false) #f)
      ((not (list? expr)) (lookup expr environment))
      ((eq? (statement-type expr) 'funcall) (interpret-function-call (cdr expr) environment currType instance (lambda (v) v) (lambda (v) v) #t))
      (else (eval-operator expr environment currType instance)))))

; Evaluate a binary (or unary) operator.  Although this is not dealing with side effects, I have the routine evaluate the left operand first and then
; pass the result to eval-binary-op2 to evaluate the right operand.  This forces the operands to be evaluated in the proper order in case you choose
; to add side effects to the interpreter
(define eval-operator
  (lambda (expr environment currType instance)
    (cond
      ((eq? 'new (operator expr)) (instantiate (operand1 expr) environment))
      ((eq? 'dot (operator expr)) (lookup-in-frame (operand2 expr) (get-instance-fieldsList (dot (operand1 expr) environment))))
      ((eq? '! (operator expr))   (not (eval-expression (operand1 expr) environment currType instance)))
      ((and (eq? '- (operator expr)) (= 2 (length expr))) (- (eval-expression (operand1 expr) environment currType instance)))
      (else (eval-binary-op2 expr (eval-expression (operand1 expr) environment currType instance) environment currType instance)))))

; Complete the evaluation of the binary operator by evaluating the second operand and performing the operation.
(define eval-binary-op2
  (lambda (expr op1value environment currType instance)
    (println (operand1 expr))
    (cond
      ((eq? '+ (operator expr)) (+ op1value (eval-expression (operand2 expr) environment currType instance)))
      ((eq? '- (operator expr)) (- op1value (eval-expression (operand2 expr) environment currType instance)))
      ((eq? '* (operator expr)) (* op1value (eval-expression (operand2 expr) environment currType instance)))
      ((eq? '/ (operator expr)) (quotient op1value (eval-expression (operand2 expr) environment currType instance)))
      ((eq? '% (operator expr)) (remainder op1value (eval-expression (operand2 expr) environment currType instance)))
      ((eq? '== (operator expr)) (isequal op1value (eval-expression (operand2 expr) environment currType instance)))
      ((eq? '!= (operator expr)) (not (isequal op1value (eval-expression (operand2 expr) environment currType instance))))
      ((eq? '< (operator expr)) (< op1value (eval-expression (operand2 expr) environment currType instance)))
      ((eq? '> (operator expr)) (> op1value (eval-expression (operand2 expr) environment currType instance)))
      ((eq? '<= (operator expr)) (<= op1value (eval-expression (operand2 expr) environment currType instance)))
      ((eq? '>= (operator expr)) (>= op1value (eval-expression (operand2 expr) environment currType instance)))
      ((eq? '|| (operator expr)) (or op1value (eval-expression (operand2 expr) environment currType instance)))
      ((eq? '&& (operator expr)) (and op1value (eval-expression (operand2 expr) environment currType instance)))
      (else (myerror "Unknown operator:" (operator expr))))))

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
(define get-closure-class operand3)

; -- abstractions for class thigns 
(define class-name operator)
(define super-name
  (lambda (classDef)
    (operand1 (operand1 classDef))))

; declaration (body) for the class
(define class-dec operand2)

(define extends?
  (lambda
      (classDef)
    (not (null? (operand1 classDef)))))

; gets the closure of a class given its binding: '((name) (closure))
(define get-class-closure
  (lambda (class-binding)
    (unbox (caadr class-binding))))
; get the closure of a clas 
(define get-closure-of
  (lambda (classname environment)
    (lookup classname environment)))

; items given the closure (unboxed)
(define get-closure-super operator)
(define get-closure-fieldsList caadr)
(define get-closure-methodsList caaddr)

(define get-instance-fieldsList operand1)


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
      ((null? l) 0)  ; should not happen
      ((eq? var (car l)) 0)
      (else (+ 1 (indexof var (cdr l)))))))

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

; if the value is an atom
(define (atom? x)
  (and (not (null? x))
       (not (pair? x))))

; to make the closure
(define make-closure
  (lambda (formal-params body environment class)
    (cons formal-params (cons body (cons environment (cons class '()))))))

; binding formal and actual parameters into a frame and returning that frame
(define bind-parameters
  (lambda (formal actual environment currType instance)
    (println "bind parameters")
    (println formal)
    (println actual)
    (if (eq? (length actual) (length formal))
        (bind-parameters-helper formal actual (newenvironment) environment currType instance)
        (myerror "Mismatched parameters and argiments."))))

(define bind-parameters-helper
  (lambda (formal actual frame environment currType instance)
    (cond
      ((null? formal)   frame)
      (else            (bind-parameters-helper (cdr formal) (cdr actual) (insert (operator formal) (eval-expression (operator actual) environment currType instance) frame) environment currType instance)))))

; updates a statement with functions to evaluate the function values
(define updateStatementWithFunctions
  (lambda (statement environment throw next updatedStatement cps-return)
    (cond
      ((null?  statement)            (cps-return statement))
      ((atom? statement)             (cps-return statement))
      ; if the statement includes a sublist, recurse
      ((list? (car statement))            (updateStatementWithFunctions (car statement) environment throw next updatedStatement (lambda (s)
                                                                                                                                           (updateStatementWithFunctions (cdr statement) environment throw next updatedStatement (lambda (s2)
                                                                                                                                                                                                                               (cps-return (cons s s2)))))))
      ; if there is a function call, evaluate the function
      ((eq? (car statement) 'funcall)  (cps-return (interpret-function-call (cdr statement) environment throw  next #t)))
      ; otherwise, keep searching through statements 
      (else (updateStatementWithFunctions (cdr statement) 
                                          environment throw next updatedStatement (lambda (s) (cps-return (cons (car statement) s))))))))


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
