#lang racket
(require "simpleParser.rkt")

(define interpret
  (lambda (filename)
    (evaluateState (parser filename) '(() ()) (lambda (v) v))))


(define evaluateState
  (lambda (tree state return)
    (cond
      ; if the tree is empty or is a single value/variable
      ((null? tree) state)
      ((atom? tree) state)

      ; if there is a nested statement
      ((list? (statementType tree)) (evaluateState (car tree)
                                    state
                                    (lambda (v) (return (evaluateState (cdr tree) v return)))))
      
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
      ((null? expression) (return 0 state))
      ; if the expression is a single number, return the value
      ((number? expression) (return expression state))
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
    (compute (car expression) state (lambda (val newState)
                                      (if val
                                          (evaluateState (cadr expression) newState (lambda (v) v))
                                          (if (null? (cddr expression))
                                              newState
                                              (evaluateState (caddr expression)newState (lambda (v) v))))))))

; while-statements
(define Mstate_while
  (lambda (expression state)
    (compute (car expression) state (lambda (val newState)
                                      (if val
                                          (Mstate_while expression (evaluateState (cadr expression) newState (lambda (v) v)))
                                           (compute (car expression) newState (lambda (val news) news)))))))

; return statement (adds binding to a special variable "return") 
(define Mstate_return
  (lambda (expression state)
    (Mvalue (cadr expression) state (lambda (value state) (print value) (addBinding 'return value state)))))
    

; calulate expression 
(define compute
  (lambda (expression state return)
   ; (print expression)
    (cond
      ; not
      ((eq? (statementType expression) '!)
       (Mvalue (leftOperand expression)
               (state
                (lambda (leftVal leftState)
                  (return (not leftVal) leftState)))))
      
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
                 (Mvalue (rightOperand expression)
                         leftState
                         (lambda (rightVal rightState)
                           (return (- leftVal rightVal) rightState))))))
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
                           (return (eq? leftVal rightVal) rightState))))))

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
      ((null? varLis) (error "variable not declared"))
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
        (if (eq? (car valLis) 'NULL)
            (begin (print valLis)
            (error "variable has not been assigned"))
            (return (car valLis)))
        (findBinding (cdr valLis) index (+ currIndex 1) (lambda (v) v)))))


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