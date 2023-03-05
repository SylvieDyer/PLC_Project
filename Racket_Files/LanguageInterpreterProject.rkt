#lang racket

;-------------------------------------------------------------------
;
;    By: Sylvie Dyer & Luis Torres
;
;-------------------------------------------------------------------
(require "simpleParser.rkt")

; interprets a file with code
(define interpret
  (lambda (filename)
    (parseCode (parser filename))))

; given a tree-structure of code, determines the state and returns its return value 
(define parseCode
  (lambda (tree)
    ; gets the return value 
    (findBindingByName 'return
                       (call/cc 
                        ; evaluates the state of the code, with a break-out method 
                        (lambda (k) (evaluateState tree '((()())) (lambda (v) v) k))))))

; determines the state of an expression 
(define evaluateState
  (lambda (tree state return break)
    (cond
      ; if the tree is empty or is a single value/variable
      ((null? tree) state)
      ((atom? tree) state)

      ; if there is a nested statement, evaluate the first statement, then the remainder
      ((list? (statementType tree)) (evaluateState (car tree)
                                                   state
                                                   (lambda (newState)
                                                     (return (evaluateState (cdr tree) newState (lambda (v) v) break)))
                                                   break))

      ((eq? (statementType tree) 'begin) (evaluateState (cdr tree) (addLayer state) (lambda (newState) (return (removeLayer newState))) break))
      
      ; --------------------------- otherwise, is some kind of statement --------------------------------
      
      ; declaring variable
      ((eq? (statementType tree) 'var) (return (Mstate_var tree state)))
      
      ; assigning variable
      ((eq? (statementType tree) '=)   (return (Mstate_assign tree state)))
 
      ; entering if statement
      ((eq? (statementType tree) 'if)  (return (Mstate_cond (cdr tree) state)))

      ; entering while statement
      ((eq? (statementType tree) 'while) (return (Mstate_while (cdr tree) state)))

      ; entering return statement (break out and return state)
      ((eq? (statementType tree) 'return) (break (Mstate_return tree state)))

      ; otherwise return the tate
      (else state))))

; evaluating the value of an expression
(define Mvalue
  (lambda (expression state return)
    (cond
      ; if the expression is empty, value is null
      ((null? expression)                  (return 'NULL state))
      
      ; if the expression is a single number, return the value
      ((number? expression)                (return expression state))
      
      ; if the variable is true or false
      ((eq? expression 'false)             (return #f state))
      ((eq? expression 'true)              (return #t state))
      
      ; if the expression is a variable, return its value 
      ((symbol? expression)                (return (findBindingByName expression state)
                                                   state))
      
      ; if the expression is a assignment, re-compute value and update state
      ((eq? (statementType expression) '=) (evaluateState expression state
                                                          (lambda (newState)
                                                            (Mvalue (rightOperand expression)
                                                                    newState
                                                                    (lambda (val s) (return val s))))
                                                          (lambda (v) v)))

      ; if the epxression has a sub list and no identifying operator / statement type
      ((list? (car expression))            (Mvalue (car expression)
                                                   (evaluateState (car expression) state (lambda (v) v) (lambda (v) v))
                                                   (lambda (val state) (return val state))))

      ; otherwise, perform calculations (need a different state being returned)
      (else                                (compute expression state (lambda (val newState) (return val newState)))))))


; variable declaration 
(define Mstate_var
  (lambda (expression state)
    ; if there is a value with the variable
    (if (pair? (cddr expression))
        ; determine the value of the associated declaration, and add binding
        (Mvalue (rightOperand expression) state (lambda (value updatedState) (addBinding (cadr expression) value updatedState)))
        ; add binding with NULL value
        (addBinding (cadr expression) 'NULL state))))
     
; assignment
(define Mstate_assign
 (lambda (expression state)
   ; check if the variable has been declared yet
   (if (isDeclared (leftOperand expression) state)
       ; if it is - cons the variable list with the updated value list
       (cons (varLis state)
             (Mvalue (rightOperand expression) state (lambda (value newState)
                                                       (replaceBinding value (valLis newState) newState
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
    ; determine if condition is true
    (Mvalue (car expression) state (lambda (val newState)
                                     (if val
                                         ; if true, determine the state of the body of the loop, and re-enter with the new state
                                         (evaluateState (cdr expression) state (lambda (v) (Mstate_while expression v)) (lambda (v) v))
                                         ; otherwise, determine the value of 
                                         (Mvalue (car expression) newState (lambda (val2 newState2) newState2)))))))

; return statement (adds binding to a special variable "return") 
(define Mstate_return
  (lambda (expression state)
    ; if 'return has already been assigned, do not re-assign
    (if (isDeclared 'return state)
        state
        (Mvalue (cdr expression) state (lambda (value newState) (addBinding 'return value newState))))))
       
; calulates an expression (mathematical or boolean)
(define compute
  (lambda (expression state return)
    (cond
      
      ; not
      ((eq? (statementType expression) '!)
       ; determine the value of the left, and only, operand
       (Mvalue (leftOperand expression)
               state
               (lambda (leftVal leftState)
                 ; return the 'not' of the value 
                  (return (not leftVal) leftState))))
      
      ; addition
      ((eq? (statementType expression) '+)
       ; determine the value of the left operand
        (Mvalue (leftOperand expression)
                state
                (lambda (leftVal leftState)
                  ; determine the value of the right operand
                  (Mvalue (rightOperand expression)
                          leftState
                          (lambda (rightVal rightState)
                            ; return the sum of the two, and the new state 
                            (return (+ leftVal rightVal) rightState))))))
      ; subtraction
      ((eq? (statementType expression) '-)
       ; determine the value of the left operand
       (Mvalue (leftOperand expression)
               state
               (lambda (leftVal leftState)
                 ; this checks if cddr doesn't exist, in which case this is a negative number
                 (if (null? (cddr expression))
                     (return (- 0 leftVal) leftState)
                     ; NOT dealing w a negative number, but rather subtraction - find value of right operand
                     (Mvalue (rightOperand expression)
                             leftState
                             (lambda (rightVal rightState)
                               ; return the difference of the two, and the new state 
                               (return (- leftVal rightVal) rightState)))))))
      ; multiplication
      ((eq? (statementType expression) '*)
       ; determine the value of the left operand
       (Mvalue (leftOperand expression)
               state
               (lambda (leftVal leftState)
                 ; determine the value of the right operand
                 (Mvalue (rightOperand expression)
                         leftState
                         (lambda (rightVal rightState)
                           ; return the product of the two, and the new state 
                           (return (* leftVal rightVal) rightState))))))

      ; division
      ((eq? (statementType expression) '/)
       ; determine the value of the left operand
       (Mvalue (leftOperand expression)
               state
               (lambda (leftVal leftState)
                 ; determine the value of the right operand
                 (Mvalue (rightOperand expression)
                         leftState
                         (lambda (rightVal rightState)
                           ; return the quotient of the two, and the new state 
                           (return (quotient leftVal rightVal) rightState))))))

      ; modulo
      ((eq? (statementType expression) '%)
       ; determine the value of the left operand
       (Mvalue (leftOperand expression)
               state
               (lambda (leftVal leftState)
                 ; determine the value of the right operand
                 (Mvalue (rightOperand expression)
                         leftState
                         (lambda (rightVal rightState)
                           ; return the remainder of the two, and the new state 
                           (return (remainder leftVal rightVal) rightState))))))
      ; less than
      ((eq? (statementType expression) '<)
       (Mvalue (leftOperand expression)
               state
               (lambda (leftVal leftState)
                 ; determine the value of the right operand
                 (Mvalue (rightOperand expression)
                         leftState
                         (lambda (rightVal rightState)
                           ; return whether leftVal < rightVal, and the new state 
                           (return (< leftVal rightVal) rightState))))))

      ; greater than 
      ((eq? (statementType expression) '>)
       ; determine the value of the left operand
       (Mvalue (leftOperand expression)
               state
               (lambda (leftVal leftState)
                 ; determine the value of the right operand
                 (Mvalue (rightOperand expression)
                         leftState
                         (lambda (rightVal rightState)
                           ; return whether leftVal > rightVal, and the new state 
                           (return (> leftVal rightVal) rightState))))))

      ; less than or equal to 
      ((eq? (statementType expression) '<=)
       (Mvalue (leftOperand expression)
               state
               (lambda (leftVal leftState)
                 ; determine the value of the right operand
                 (Mvalue (rightOperand expression)
                         leftState
                         (lambda (rightVal rightState)
                           ; return whether leftVal <= rightVal, and the new state 
                           (return (<= leftVal rightVal) rightState))))))

      ; greater than or equal to
      ((eq? (statementType expression) '>=)
       ; determine the value of the left operand
       (Mvalue (leftOperand expression)
               state
               (lambda (leftVal leftState)
                 ; determine the value of the right operand
                 (Mvalue (rightOperand expression)
                         leftState
                         (lambda (rightVal rightState)
                           ; return whether leftVal >= rightVal, and the new state 
                           (return (>= leftVal rightVal) rightState))))))

      ; equal
      ((eq? (statementType expression) '==)
       ; determine the value of the left operand
       (Mvalue (leftOperand expression)
               state
               (lambda (leftVal leftState)
                 ; determine the value of the right operand
                 (Mvalue (rightOperand expression)
                         leftState
                         (lambda (rightVal rightState)
                           ; return if leftVal == rightVal
                           (return (if (eq? leftVal rightVal) ; if statement converts 'true and 'false into #t / #f
                                       #t
                                       #f)
                                       rightState))))))

      ; not equal
      ((eq? (statementType expression) '!=)
       ; determine the value of the left operand
       (Mvalue (leftOperand expression)
               state
               (lambda (leftVal leftState)
                 ; determine the value of the right operand
                 (Mvalue (rightOperand expression)
                         leftState
                         (lambda (rightVal rightState)
                           ; return whether leftVal < rightVal, and the new state 
                           (return (not (eq? leftVal rightVal)) rightState))))))

      ; or
      ((eq? (statementType expression) '||)
       ; determine the value of the left operand
       (Mvalue (leftOperand expression)
               state
               (lambda (leftVal leftState)
                 ; determine the value of the right operand
                 (Mvalue (rightOperand expression)
                         leftState
                         (lambda (rightVal rightState)
                           ; return leftVal || rightVal are true, and the new state 
                           (return (or leftVal rightVal) rightState))))))

      ;and
      ((eq? (statementType expression) '&&)
       ; determine the value of the left operand
       (Mvalue (leftOperand expression)
               state
               (lambda (leftVal leftState)
                 ; determine the value of the right operand
                 (Mvalue (rightOperand expression)
                         leftState
                         (lambda (rightVal rightState)
                           ; return whether leftVal && rightVal are true , and the new state
                           (return (and leftVal rightVal) rightState))))))

      ; otherwise, determine the value of the car of the expression and return
      (else (Mvalue (car expression) state (lambda (value newState) (return value newState))))
      )))

 ;returns true if a variable has been declared already 
(define isDeclared
  (lambda (var state)
   ; (indexOfVariable var state 0)))
   (not (eq? (cadr (indexOfVariable var state 0)) -1))))

; indexOfVaiable - returns the layer and index of a variable (layer index)
(define indexOfVariable
  (lambda (var state layer)
    (call/cc
     ; call the helper with the given inputs
     (lambda (break) (indexOfVariable-break var (caar state) 0 layer
                                            ; make continuation function call the rest of the state with updated layer
                                            (lambda (result) (if (null? (cdr state))
                                                                 result
                                                                 (indexOfVariable var (cdr state) (+ 1 layer))))
                                            ; pass in the break 
                                            break)))))

; returns the layer and index of a variable, incorporating the break 
(define indexOfVariable-break
  (lambda (var varLis index layer return break)
    (cond
      ; if nothing has been found, return default list
      ((null? varLis)          (return '(-1 -1)))
      ; otherwise, immediately return the layer and index
      ((eq? var (car varLis))  (break (cons layer (cons index '()))))
      ; otherwise, continue searching 
      (else                    (indexOfVariable-break var (cdr varLis) (+ 1 index) layer return break)))))
                               
                               
; adds a variable and value pair to the state
(define addBinding
  (lambda (var value state)
    ; if the variable has already been declared... 
    (if (isDeclared var state)
        ; ... can't redeclare
        (error "Variable has already been declared: " var)
        ; otherwise, combine first layer to rest of state where... 
        (cons
         ; add binding to the first layer of the state
         (cons (cons var (varLis (currentLayer state)))
               (cons (cons value (valLis (currentLayer state))) '()))
         ; rest of the state (unchanged) 
         (cdr state)))))

; replace an existing binding of a variable, at a given index, with the new value
(define replaceBinding 
  (lambda (value valLis index currIndex return)
    ; if found the index
    (cond
      ; Are in the same layer
      ((eq? (car index) (car currIndex))
       (if (eq? (cdr index) (cdr currIndex))
           (return (cons value (cdr valLis)))
           (replaceBinding value valLis state index (cons (car currIndex) (cons (+ (cadr currIndex) 1) '())))))
      ; Are in different layer
      (else (replaceBinding value ( (removeLayer state)
    (if (eq? (car index) (car currIndex))
        ; replace the value and return
        (return (cons value (cdr valLis)))
        ; continue searching
        (replaceBinding value (cdr valLis) index (+ currIndex 1) (lambda (v) (return (cons (car valLis) v)))))))))))

; get the value of a variable, at a given index
(define findBinding
  (lambda (valLis index currIndex return)
    ; if the current index is the same:
    (if (eq? index currIndex)
        ; return the value 
        (parseValue (car valLis))
         ; otherwise continue searcihing 
        (findBinding (cdr valLis) index (+ currIndex 1) return))))

; get the value of a variable given its name
(define findBindingByName
  (lambda (name state)
    (findBindingByNameHelper name (varLis state) (valLis state))))

; helper method that splits the state to recursively search
(define findBindingByNameHelper
 (lambda (name varLis valLis)
    (cond
      ; if either list is empty, variable has not been declared- throw error 
      ((or (null? varLis)       (null? valLis)) (error "Variable has not been declared yet: " name))
      ; if the name is found, return coresponding value
      ((eq? name (car varLis)) (parseValue (car valLis)))
      ; otherwise keep searching
      (else                    (findBindingByNameHelper name (cdr varLis) (cdr valLis))))))

; returns a proper value or error
(define parseValue
  (lambda (value)
    (cond
      ((eq? value 'NULL)     (error "Variable has not been assigned"))
      ((eq? value #t)        'true)
      ((eq? value #f)        'false)
      (else                  value))))
                             
; ------Abstractions--------------------

; Adds a layer to the state
(define addLayer
  (lambda (state)
    (cons '(()()) state)))

; Removes the top layer from the given state
(define removeLayer
  (lambda (state)
    (cdr state)))

; Return the current (first) layer of the state
(define currentLayer
  (lambda (state)
    (car state)))

; Gets the specified layer
(define getLayer
  (lambda (layer state)
    (getLayer-helper layer 0 state (lambda (v) v))))

(define getLayer-helper
  (lambda (layer currLayer state return)
    (if (eq? layer currLayer)
        (return (car state))
        (getLayer-helper layer (+ 1 currLayer) (cdr state) return))))

; Replaces the specified layer
(define replaceLayer
  (lambda (layer newLayer state)
    (replaceLayer-helper layer 0 state newLayer (lambda (v) v))))
    
(define replaceLayer-helper
  (lambda (layer currLayer state newLayer return)
    (if (eq? layer currLayer)
        (return (cons newLayer (cdr state)))
        (replaceLayer-helper layer (+ 1 currLayer) (cdr state) newLayer (lambda (restOfState) (return (cons (car state) restOfState)))))))
    
    
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