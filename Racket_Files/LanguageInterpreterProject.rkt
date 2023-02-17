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

      ; entering sub-tree 
      ((list? (statementType tree)) (evaluateState (car tree)
                                    state
                                    (lambda (v) (return (evaluateState (cdr tree) v return)))))
      
      ; declaring variable
      ((eq? (statementType tree) 'var) (return (Mstate_var tree state)))
      
      ; assigning variable
      ((eq? (statementType tree) '=)  (return (Mstate_assign tree state)))

      
      ; entering if statement

      ; entering while statement
      (else state)
      
  )))



; returns the statement type 
(define statementType
  (lambda (expression)
    (car expression)))



; evaluating the value of an expression
(define Mvalue
  (lambda (expression state return)

    (cond
      ((null? expression) (return 0 state))
      ; if the expression is a single number, return the value
      ((number? expression) (return expression state))
      ; if the expression is a variable, find and return the assigned value
      ((symbol? expression) (print "search for value"))

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
                                                     (print state)
                                                     (return val state))))

      ; otherwise, perform calculations (need a different state being returned)
      (else (return (compute expression state (lambda (v) (print "?") (return v state))) state)))))


; calulate
(define compute
  (lambda (expression state return)
   ; (print expression)
    (cond
      ((eq? (statementType expression) '+)
       ;(+ (Mvalue (leftOperand expression) state (lambda (leftVal leftState) leftVal))
       ; (Mvalue (rightOperand expression) state (lambda (leftVal leftState) leftVal)))
       (Mvalue (leftOperand expression) state (lambda (leftVal leftState)
                                                (Mvalue (rightOperand expression)
                                                        leftState
                                                        (lambda (rightVal rightState)
                                                          (+ leftVal rightVal))))))
                                           
                                          
                                           )))

                                          

; right operand
(define rightOperand
  (lambda (expression)
    (caddr expression)))

; left operand
(define leftOperand
  (lambda (expression)
    (cadr expression)))


; if the value is an atom
(define (atom? x)
  (and (not (null? x))
       (not (pair? x))))

; variable declaration 
(define Mstate_var
  (lambda (expression state)
    (if (pair? (cddr expression))
       
        (addBinding (cadr expression) (caddr expression) state)
        (addBinding (cadr expression) 'NULL state))))
        ;(cons (cons (cadr expression) (cons (caddr expression) '())) state)
        ;(cons (cons (cadr expression) '()) state))))

; bindings are only added when declaring, so no need for checks!
; add a state, value pair 
(define addBinding
  (lambda (var value state)
    (cons (cons var (car state))
          (cons (cons value (cadr state)) '()))))
     
; assignment
(define Mstate_assign
 (lambda (expression state)
   ;(print expression)
   (cons (car state)
         (cons (replaceBinding (rightOperand expression)
                               (cadr state)
                               (isDeclared (leftOperand expression) (car state) (lambda (v) v))
                               0
                               (lambda (v) v)) '()))))
                        

   
   ; (if ((isDeclared (cadr expression) (cadr state) (lambda (v) v))
     


; determine if a variable has been declared and return its index (-1 if not)
(define isDeclared
  (lambda (var varLis return)
    (cond
      ; if there are no delcared variables, var is undeclared
      ((null? varLis) (error "variable not declared"))
      ; otherwise, check to see if the variable is found
      ((eq? var (car varLis)) (return 0))
      ; otherwise, cps recurse
      (else (isDeclared var (cdr varLis) (lambda (v) (return (+ v 1))))))))

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
        (return (car valLis))
        (findBinding (cdr valLis) index (+ currIndex 1) (lambda (v) v)))))

;------------------------------------------------- 


; OLD
(define declareVariable
  (lambda (variable state)
     (cons (cons variable '()) state)
    ))

; OLD
(define declareVariableValue
  (lambda (variable value state)
    (cons (cons variable (cons value '())) state)
    ))

    