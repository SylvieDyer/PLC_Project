#lang racket
(require "simpleParser.rkt")

(define interpret
  (lambda (filename)
    (evaluateState (parser filename) '((return) (RETURNVAL)) (lambda (finalState) (print finalState) (findBindingByName 'return finalState (lambda (v) v))))))

(define evaluateState
  (lambda (tree state return)
    (cond
      ; if the tree is empty or is a single value/variable
      ((null? tree) state)
      ((atom? tree) state)

      ; if there is a nested statement
      ((list? (statementType tree)) (evaluateState (car tree)
                                    state
                                    (lambda (newState)
                                      (return (evaluateState (cdr tree) newState (lambda (v) v) )))
                                    ))
      
      ; --------------------------- otherwise, is some kind of statement --------------------------------
      
      ; declaring variable
      ((eq? (statementType tree) 'var) (return (Mstate_var tree state)))
      
      ; assigning variable
      ((eq? (statementType tree) '=)  (return (Mstate_assign tree state)))
 
      ; entering if statement
      ((eq? (statementType tree) 'if) (return (Mstate_cond (cdr tree) state)))

      ; entering while statement
      ((eq? (statementType tree) 'while) (return (Mstate_while (cdr tree) state)))

      ; entering return statement
      ((eq? (statementType tree) 'return) (return (Mstate_return tree state)))
      
      (else state)
  )))

; evaluating the value of an expression
(define Mvalue
  (lambda (expression state return)

    (cond
      ((null? expression) (return 'NULL state))
      ; if the expression is a single number, return the value
      ((number? expression) (return expression state))
      ; if the variable is true or false
      ((eq? expression 'false) (return #f state))
      ((eq? expression 'true) (return #t state))
      ;((or (eq? expression 'false) (eq? expression 'true)) (return expression state))
      ; if the expression is a variable, determine if it's been declared
      ((symbol? expression) (return (isDeclared expression
                                                (car state)
                                                ; then find the value of the variable 
                                                (lambda (index) (findBinding (cadr state)
                                                                             index
                                                                             0
                                                                             (lambda (val) val))))
                                    state))
      ; (findBinding (cadr state) (isDeclared expression (car state) (lambda (v) v)) 0 (lambda (x) x)))

      ; --------------- not needed?
      ; if the expression has a nested expression needing evaluation -> Find the value of the expression 
      ; ((list? (statementType expression)) (Mvalue (car expression) (evaluateState (car expression) state (lambda (v) v)) return))
      ;((list? (car expression)) (evaluateState (car expression) state (lambda (state) (Mvalue (car expression) state (lambda (value state) (return value state))))))
      ; ----------------

      ; if the expression is a assignment, update state
      ((eq? (statementType expression) '=) (Mvalue (rightOperand expression)
                                                   (evaluateState expression state (lambda (v) v))
                                                   (lambda (val state)
                                                     ; updated state!
                                                    ; (print state)
                                                     (return val state))))

      ; otherwise, perform calculations (need a different state being returned)
      (else (compute expression state (lambda (v state) (return v state)))))))


; variable declaration 
(define Mstate_var
  (lambda (expression state)
    (if (pair? (cddr expression))
        (Mvalue (rightOperand expression) state (lambda (value updatedState) (addBinding (cadr expression) value updatedState)))
        (addBinding (cadr expression) 'NULL state))))
        
        ;(cons (cons (cadr expression) (cons (caddr expression) '())) state)
        ;(cons (cons (cadr expression) '()) state))))

     
; assignment
(define Mstate_assign
 (lambda (expression state)
   (cons (car state)
         (cons (replaceBinding (Mvalue (rightOperand expression) state (lambda(value state) value))
                               (cadr state)
                               (isDeclared (leftOperand expression) (car state) (lambda (v) v))
                               0
                               (lambda (v) v)) '()))))     

; if-statements
(define Mstate_cond
  (lambda (expression state)
   
  ;  (print (Mvalue expression state (lambda (v1) v1)))

    ; determine if the statement is true or false
;    (if (atom? (car expression))
;        (Mvalue (car expression) state (lambda (val newState)
;                                         (if val
;                                             (evaluateState (cadr expression) newState (lambda (v) v) )
;                                             (if (null? (cddr expression))
;                                                 newState
;                                                 (evaluateState (caddr expression) newState (lambda  (v) v))))))
        (Mvalue (car expression) state (lambda (val newState)
                                      (if val
                                          ; if true, go through the if-statement
                                          (evaluateState (cadr expression) newState (lambda (v) v) )
                                          ; otherwise, check if there is an else condition
                                          (if (null? (cddr expression))
                                              ; if not, return state
                                              newState
                                              ; if there is, go through the else-statement
                                              (evaluateState (caddr expression) newState (lambda (v) v) )))
                                          
                                      ))))


; while-statements
(define Mstate_while
  (lambda (expression state)
    (Mvalue (car expression) state (lambda (val newState)
                                     (if val
                                         (evaluateState (cdr expression) newState (lambda (v) v))
                                         (Mvalue (car expression) newState (lambda (val2 newState2) newState2)))))))
  ;  (compute (car expression) state (lambda (val newState)
                                    ;  (if val
                                         ; (Mstate_while expression (evaluateState (cadr expression) newState (lambda (v) v)))
                                          ; (compute (car expression) newState (lambda (val news) news)))))))

; return statement (adds binding to a special variable "return") 
(define Mstate_return
  (lambda (expression state)
    (if (eq? (findBindingByName 'return state (lambda (val) val)) 'RETURNVAL)
        (Mstate_assign (cons '= expression) state)
        state
    )))

; calulate expression 
(define compute
  (lambda (expression state return)
    (cond
      ; not
      ((eq? (statementType expression) '!)
       (Mvalue (leftOperand expression)
               state
               (lambda (leftVal leftState)
                  (return (not leftVal) leftState))))
      
      ; addition
      ((eq? (statementType expression) '+)
        (Mvalue (leftOperand expression)
                state
                (lambda (leftVal leftState)
                  (Mvalue (rightOperand expression)
                          leftState
                          (lambda (rightVal rightState)
                            (return (+ leftVal rightVal) rightState))))))
      ; subtraction
      ((eq? (statementType expression) '-)
       (Mvalue (leftOperand expression)
               state
               (lambda (leftVal leftState)
                 ; this checks if cddr doesn't exist, in which case this is a negative number
                 (if (null? (cddr expression))
                     (return (- 0 leftVal) leftState)
                     ; NOT dealing w a negative number, but rather subtraction
                     (Mvalue (rightOperand expression)
                             leftState
                             (lambda (rightVal rightState)
                               (return (- leftVal rightVal) rightState)))))))
      ; multiplication
      ((eq? (statementType expression) '*)
       (Mvalue (leftOperand expression)
               state
               (lambda (leftVal leftState)
                 (Mvalue (rightOperand expression)
                         leftState
                         (lambda (rightVal rightState)
                           (return (* leftVal rightVal) rightState))))))

      ; division
      ((eq? (statementType expression) '/)
       (Mvalue (leftOperand expression)
               state
               (lambda (leftVal leftState)
                 (Mvalue (rightOperand expression)
                         leftState
                         (lambda (rightVal rightState)
                           (return (quotient leftVal rightVal) rightState))))))

      ; modulo
      ((eq? (statementType expression) '%)
       (Mvalue (leftOperand expression)
               state
               (lambda (leftVal leftState)
                 (Mvalue (rightOperand expression)
                         leftState
                         (lambda (rightVal rightState)
                           (return (remainder leftVal rightVal) rightState)))))) ; issue here?
      ; less than
      ((eq? (statementType expression) '<)
       (Mvalue (leftOperand expression)
               state
               (lambda (leftVal leftState)
                 (Mvalue (rightOperand expression)
                         leftState
                         (lambda (rightVal rightState)
                           (return (< leftVal rightVal) rightState))))))

      ; greater than 
      ((eq? (statementType expression) '>)
       (Mvalue (leftOperand expression)
               state
               (lambda (leftVal leftState)
                 (Mvalue (rightOperand expression)
                         leftState
                         (lambda (rightVal rightState)
                           (return (> leftVal rightVal) rightState))))))

      ; less than or equal to 
      ((eq? (statementType expression) '<=)
       (Mvalue (leftOperand expression)
               state
               (lambda (leftVal leftState)
                 (Mvalue (rightOperand expression)
                         leftState
                         (lambda (rightVal rightState)
                           (return (<= leftVal rightVal) rightState))))))

      ; greater than or equal to
      ((eq? (statementType expression) '>=)
       (Mvalue (leftOperand expression)
               state
               (lambda (leftVal leftState)
                 (Mvalue (rightOperand expression)
                         leftState
                         (lambda (rightVal rightState)
                           (return (>= leftVal rightVal) rightState))))))

      ; equal
      ((eq? (statementType expression) '==)
       (Mvalue (leftOperand expression)
               state
               (lambda (leftVal leftState)
                 (Mvalue (rightOperand expression)
                         leftState
                         (lambda (rightVal rightState)
                           (return (if (eq? leftVal rightVal) ; if statement is to store as 'true 'false instead of #t #f
                                       #t
                                       #f)
                                       rightState))))))

      ; not equal
      ((eq? (statementType expression) '!=)
       (Mvalue (leftOperand expression)
               state
               (lambda (leftVal leftState)
                 (Mvalue (rightOperand expression)
                         leftState
                         (lambda (rightVal rightState)
                           (return (not (eq? leftVal rightVal)) rightState))))))

      ; or
      ((eq? (statementType expression) '||)
       (Mvalue (leftOperand expression)
               state
               (lambda (leftVal leftState)
                 (Mvalue (rightOperand expression)
                         leftState
                         (lambda (rightVal rightState)
                           (return (or leftVal rightVal) rightState))))))

      ;and
      ((eq? (statementType expression) '&&)
       (Mvalue (leftOperand expression)
               state
               (lambda (leftVal leftState)
                 (Mvalue (rightOperand expression)
                         leftState
                         (lambda (rightVal rightState)
                           (return (and leftVal rightVal) rightState))))))
      (else (return (car expression)))
      )))

; determine if a variable has been declared and return its index (error if not)
(define isDeclared
  (lambda (var varLis return)
    (cond
      ; if there are no delcared variables, var is undeclared
      ((null? varLis) (error "variable not declared: " var))
      ; otherwise, check to see if the variable is found
      ((eq? var (car varLis)) (return 0))
      ; otherwise, cps recurse
      (else (isDeclared var (cdr varLis) (lambda (v) (return (+ v 1))))))))

; add a state, value pair (called when declaring)
(define addBinding
  (lambda (var value state)
    (cons (cons var (car state))
          (cons (cons value (cadr state)) '()))))

; replace an existing binding of a variable, at a given index, with the new value
(define replaceBinding
  (lambda (value valLis index currIndex return)
    (if (eq? index currIndex)
        (return (cons value (cdr valLis)))
        (replaceBinding value (cdr valLis) index (+ currIndex 1) (lambda (v) (return (cons (car valLis) v)))))))


; get the value of a variable, at a given index
(define findBinding
  (lambda (valLis index currIndex return)
    (if (eq? index currIndex)
        (cond
          ((eq? (car valLis) 'NULL) (error "variable has not been assigned" ))
          ((eq? (car valLis) #f) (return 'false))
          ((eq? (car valLis) #t) (return 'true))
          (else (return (car valLis))))
        (findBinding (cdr valLis) index (+ currIndex 1) return))))

; get the value of a variable given its name
(define findBindingByName
  (lambda (name state return)
    (isDeclared name (car state) (lambda (index) (findBinding (cadr state) index 0 (lambda (val) val))))))

; ------Abstractions--------------------

; returns the statement type 
(define statementType
  (lambda (expression)
    (car expression)))

; right operand (pre-fix form)
(define rightOperand
  (lambda (expression)
    (caddr expression)))

; left operand (pre-fix form)
(define leftOperand
  (lambda (expression)
    (cadr expression)))

; if the value is an atom
(define (atom? x)
  (and (not (null? x))
       (not (pair? x))))