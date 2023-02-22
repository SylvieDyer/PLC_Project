#lang racket

;-------------------------------------------------------------------
;
;    By: Sylvie Dyer & Luis Torres
;
;-------------------------------------------------------------------
(require "simpleParser.rkt")

(define interpret
  (lambda (filename)
    (parseCode (parser filename))))
   
(define parseCode
  (lambda (tree)
   (findBindingByName 'return
    (call/cc
     (lambda (k) (evaluateState tree '(()()) (lambda (v) v) k))))))
    
(define evaluateState
  (lambda (tree state return break)
    (cond
      ; if the tree is empty or is a single value/variable
      ((null? tree) state)
      ((atom? tree) state)

      ; if there is a nested statement, evaluate that statement, then the rest
      ((list? (statementType tree)) (evaluateState (car tree)
                                    state
                                    (lambda (newState)
                                      (return (evaluateState (cdr tree) newState (lambda (v) v) break)))
                                    break
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
      ((eq? (statementType tree) 'return) (break (Mstate_return tree state)))

      ; otherwise return the tate
      (else state))))

; evaluating the value of an expression
(define Mvalue
  (lambda (expression state return)
    (cond
      ; if the expression is empty, value is null
      ((null? expression) (return 'NULL state))
      ; if the expression is a single number, return the value
      ((number? expression) (return expression state))
      ; if the variable is true or false
      ((eq? expression 'false) (return #f state))
      ((eq? expression 'true) (return #t state))
      ; if the expression is a variable, return its value 
      ((symbol? expression)  (return (findBindingByName expression state)
                                    state))
      
      ; if the expression is a assignment, re-compute value and update state
      ((eq? (statementType expression) '=) (evaluateState expression state (lambda (newState) (Mvalue (rightOperand expression) newState (lambda (val s) (return val s)))) (lambda (v) v)))

      ; if the epxression has a sub list (ONLY CASE SHOULD BE "return 6 * 20 +40;" etc --> otherwise there would be an operator in prefix)?
      ((list? (car expression)) (Mvalue (car expression)
                                        (evaluateState (car expression) state (lambda (v) v) (lambda (v) v))
                                        (lambda (val state) (return val state))))

      ; otherwise, perform calculations (need a different state being returned)
      (else (compute expression state (lambda (val newState) (return val newState)))))))


; variable declaration 
(define Mstate_var
  (lambda (expression state)
    (if (pair? (cddr expression))
        (Mvalue (rightOperand expression) state (lambda (value updatedState) (addBinding (cadr expression) value updatedState)))
        (addBinding (cadr expression) 'NULL state))))
     
; assignment
(define Mstate_assign
 (lambda (expression state)
   ; check if the variable has been declared yet
   (if (isDeclared (leftOperand expression) state)
       ; if it is - cons the variable list with the updated value list
       (cons (varLis state)
             (Mvalue (rightOperand expression) state (lambda (value newState)
                                                       (replaceBinding value (valLis newState)
                                                                       (indexOfVariable (leftOperand expression) newState)
                                                                       0
                                                                       (lambda (v) (cons v '()))))))
       ; otherwise throw an error
       (error "Cannot asign variable: variable has not been declared yet: " (leftOperand expression)))))

; if-statements
(define Mstate_cond
  (lambda (expression state)
    ; determine the boolean value of the condition
    (Mvalue (car expression) state (lambda (val newState)
                                     (if val
                                         ; if true, go through the if-statement
                                         (evaluateState (cadr expression) newState (lambda (v) v) (lambda (v) v))
                                         ; otherwise, check if there is an else condition
                                         (if (null? (cddr expression))
                                             ; if not, return state
                                             newState
                                             ; if there is, go through the else-statement
                                             (evaluateState (caddr expression) newState (lambda (v) v) (lambda (v) v))))))))


; while-statements
(define Mstate_while
  (lambda (expression state)
    ; check the condition
    (Mvalue (car expression) state (lambda (val newState)
                                     (if val
                                         (evaluateState (cdr expression) state (lambda (v) (Mstate_while expression v)) (lambda (v) v))
                                         (Mvalue (car expression) newState (lambda (val2 newState2) newState2)))))))

; return statement (adds binding to a special variable "return") 
(define Mstate_return
  (lambda (expression state)
    ; if 'return has already been assigned, do not re-assign
    (if (isDeclared 'return state)
        state
        (Mvalue (cdr expression) state (lambda (value newState) (addBinding 'return value newState))))))
       
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
                           (return (remainder leftVal rightVal) rightState))))))
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
                           (return (if (eq? leftVal rightVal) ; if statement converts 'true and 'false into #t / #f
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
      
      (else (Mvalue (car expression) state (lambda (value newState) (return value newState))))
      )))

; returns true if a variable has been declared already 
(define isDeclared
  (lambda (var state)
    (not (eq? (indexOfVariable var state) -1))))

; indexOfVarible - returns the index of a variable
(define indexOfVariable
  (lambda (var state)
    (call/cc
     (lambda (k) (indexOfVariable-break var (car state) 0 k)))))

(define indexOfVariable-break
  (lambda (var varLis index break)
    (cond
      ; if variable has not been declared, immediately return -1 
      ((null? varLis) (break -1))
      ; if the variable has been found, immediately return the index
      ((eq? var (car varLis)) (break index))
      ; otherwise keep searching
      (else (indexOfVariable-break var (cdr varLis) (+ 1 index) break)))))

; adds a variable and value pair to the state
(define addBinding
  (lambda (var value state)
    ; if the variable has already been declared, shouldn't be declared again
    (if (isDeclared var state)
        (error "Variable has already been declared: " var)
        (cons (cons var (varLis state))
              (cons (cons value (valLis state))'())))))

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
  (lambda (name state)
    (findBindingByNameHelper name (varLis state) (valLis state))))

(define findBindingByNameHelper
 (lambda (name varLis valLis)
    (cond
      ; if either list is empty, variable has not been declared- throw error 
      ((or (null? varLis) (null? valLis)) (error "Variable has not been declared yet: " name))
      ; if the name is found, return coresponding value
      ((eq? name (car varLis)) (parseValue (car valLis)))
      ; otherwise keep searching
      (else (findBindingByNameHelper name (cdr varLis) (cdr valLis))))))

; returns a proper value or error
(define parseValue
  (lambda (value)
    (cond
      ((eq? value 'NULL)     (error "Variable has not been assigned"))
      ((eq? value #t)        'true)
      ((eq? value #f)        'false)
      (else                  value))))
                             
; ------Abstractions--------------------

; returns the state's variables
(define varLis
  (lambda (state)
    (car state)))

; returns the state's values
(define valLis
  (lambda (state)
    (cadr state)))

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