#lang racket

;-------------------------------------------------------------------
;
;    By: Sylvie Dyer & Luis Torres
;
;-------------------------------------------------------------------
(require "simpleParser.rkt")
(provide (all-defined-out))

; interprets a file with code
(define interpret
  (lambda (filename)
    (interpretCode (parser filename))))

; given a tree-structure of code, determines the state and returns its return value 
(define interpretCode
  (lambda (tree)
    ; gets the return value
    ;(call/cc (lambda (k) (evaluateState tree '((()())) (lambda (v) v) k)))));(lambda (v2) (findBindingByName 'return k))))) 
    (findBindingByName 'return
                       (call/cc 
                        ; evaluates the state of the code, with a break-out method 
                        (lambda (k) (evaluateState tree '((()())) (lambda (v) v) (lambda (v) v) (lambda (v) v) (lambda (v) v) k))))))

; determines the state of an expression 
(define evaluateState
  (lambda (tree state return continue break throw returnBreak)
    (cond
      ; if the tree is empty or is a single value/variable
      ((null? tree) state)
      ((atom? tree) state)

      ; if there is a nested statement, evaluate the first statement, then the remainder
      ((list? (statementType tree)) (evaluateState (car tree)
                                                   state
                                                   (lambda (newState)
                                                     (return (evaluateState (cdr tree) newState (lambda (v) v) continue break throw returnBreak)))
                                                   continue
                                                   break
                                                   throw
                                                   returnBreak))

      ((eq? (statementType tree) 'begin) (evaluateState (cdr tree) (addLayer state) (lambda (newState) (return (removeLayer newState))) continue break throw returnBreak))
      
      ; --------------------------- otherwise, is some kind of statement --------------------------------
      
      ; declaring variable
      ((eq? (statementType tree) 'var) (return (Mstate_var tree state continue break throw returnBreak)))
      
      ; assigning variable
      ((eq? (statementType tree) '=)   (return (Mstate_assign tree state continue break throw returnBreak)))
 
      ; entering if statement
      ((eq? (statementType tree) 'if)  (return (Mstate_cond (cdr tree) state continue break throw returnBreak)))

      ; entering while statement
      ((eq? (statementType tree) 'while) (return (Mstate_while (cdr tree) state continue (lambda (v) (call/cc (lambda (k) (k (return (removeLayer v)))))) throw returnBreak)))

      ; entering return statement (break out and return state)
      ((eq? (statementType tree) 'return) (returnBreak (Mstate_return tree state continue break throw returnBreak)))

      ; entering a try statement
      ((eq? (statementType tree) 'try) (evaluateState (cadr tree) (addLayer state)
                                                              ; return to go to finally
                                                              (lambda (newState)
                                                                (cond
                                                                  ; check if there is only a finally & no catch
                                                                  ((eq? (caaddr tree) 'finally) (evaluateState (cadr (caddr tree)) (removeLayer newState) return continue break throw returnBreak))
                                                                  ; catch exist, does finally exist?                                                                                                
                                                                  ((pair? (cdddr tree)) (evaluateState (cadr (cadddr tree)) (removeLayer newState) return continue break throw returnBreak))
                                                                  ; finally doesn't exist, so just return the newState
                                                                  (else (removeLayer newState)))) 
                                                              ; new continue to go to finally
                                                              (lambda (newState)
                                                                (cond
                                                                  ; check if there is only a finally & no catch
                                                                  ((eq? (caaddr tree) 'finally) (evaluateState (cadr (caddr tree)) (removeLayer newState)return continue break throw returnBreak))
                                                                  ; catch exist, does finally exist?                                                                                                
                                                                  ((pair? (cdddr tree)) (evaluateState (cadr (cadddr tree)) (removeLayer newState) return continue break throw returnBreak))
                                                                  ; finally doesn't exist, so just return the newState
                                                                  (else (removeLayer newState)))) 
                                                              ; new break to execute finally statement
                                                              break
                                                      ;        (lambda (newState)
                                                      ;          (cond
                                                      ;            ; check if there is only a finally & no catch
                                                      ;            ((eq? (caaddr tree) 'finally) (evaluateState (caddr tree) (removeLayer newState) continue break throw returnBreak))
                                                      ;            ; catch exist, does finally exist?                                                                                                
                                                      ;            ((pair? (cdddr tree)) (evaluateState (caddr tree) (removeLayer newState) continue break throw returnBreak))
                                                      ;            ; finally doesn't exist, so just return the newState
                                                      ;            (else (removeLayer newState)))) 
                                                              ; new throw to throw errors sylvie makes
                                                              (lambda (newState)
                                                                (if (eq? (caaddr tree) 'catch)
                                                                    (evaluateState (caddr (caddr tree)) (removeLayer newState) return continue break throw returnBreak)
                                                                    (print "No Catch stupid")))
                                                              returnBreak))

      ; saw a break statement
      ((eq? (statementType tree) 'break) (break state))

      ; saw a continue statement
      ((eq? (statementType tree) 'continue) (continue state))

      ; saw a throw
      ((eq? (statementType tree) 'throw) (throw (replaceLayer (getNumLayers state) (cons (cons 'e (varLis (getLayer (getNumLayers state) state))) (cons (cons (cadr tree) (valLis (getLayer (getNumLayers state) state))) '())) state)) );(println (replaceLayer 0 (addBinding 'e (cadr tree) (getLayer 0 state)) state)) (throw (replaceLayer 0 (addBinding 'e (cadr tree) state) state)));(print ) (throw (addBinding 'e (cdr state) state))) ;(begin (print tree) (throw state)))
      ;((eq? (statementType tree) 'throw) (print (replaceLayer (getNumLayers state) (addBinding 'e 10 (getLayer (getNumLayers state) state)) state))) 
      ; otherwise return the tate
      (else state))))

; evaluating the value of an expression
(define Mvalue
  (lambda (expression state return continue break throw returnBreak)
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
                                                                    (lambda (val s) (return val s)) continue break throw returnBreak))
                                                          continue break throw
                                                          returnBreak))

      ; if the epxression has a sub list and no identifying operator / statement type
      ((list? (car expression))            (Mvalue (car expression)
                                                   (evaluateState (car expression) state (lambda (v) v) continue break throw returnBreak)
                                                   (lambda (val state) (return val state)) continue break throw returnBreak))

      ; otherwise, perform calculations (need a different state being returned)
      (else                                (compute expression state (lambda (val newState) (return val newState)) continue break throw returnBreak)))))


; variable declaration 
(define Mstate_var
  (lambda (expression state continue break throw returnBreak)
    ; if there is a value with the variable
    (if (pair? (cddr expression))
        ; determine the value of the associated declaration, and add binding
        (Mvalue (rightOperand expression) state (lambda (value updatedState) (addBinding (cadr expression) value updatedState)) continue break throw returnBreak)
        ; add binding with NULL value
        (addBinding (cadr expression) 'NULL state))))
     
; assignment
(define Mstate_assign
 (lambda (expression state continue break throw returnBreak)
   ; calculate the value of the assignment
   (Mvalue (rightOperand expression) state (lambda (value newState)
                                             ; account for side effects, and replace the binding 
                                             (replaceBinding (leftOperand expression) value newState))
           continue break throw returnBreak)))

; if-statements
(define Mstate_cond
  (lambda (expression state continue break throw returnBreak)
    ; determine the boolean value of the condition
    (Mvalue (car expression) state (lambda (val newState)
                                     (if val
                                         ; if true, go through the if-statement
                                         (evaluateState (cadr expression) newState (lambda (v) v) continue break throw returnBreak)
                                         ; otherwise, check if there is an else condition
                                         (if (null? (cddr expression))
                                             ; if not, return state
                                             newState
                                             ; if there is, go through the else-statement
                                             (evaluateState (caddr expression) newState (lambda (v) v) continue break throw returnBreak))))
                                          continue break throw returnBreak)))


; while-statements
(define Mstate_while
  (lambda (expression state continue break throw returnBreak)
    ; determine if condition is true
    (Mvalue (car expression) state (lambda (val newState)
                                     (if val
                                         ; if true, determine the state of the body of the loop, and re-enter with the new state
                                         (evaluateState (cdr expression) newState (lambda (v) (Mstate_while expression v continue break throw returnBreak)) continue break throw returnBreak)
                                         ; otherwise, determine the value of 
                                         (Mvalue (car expression) newState (lambda (val2 newState2) newState2) continue break throw returnBreak))) continue break throw returnBreak)))

; return statement (adds binding to a special variable "return") 
(define Mstate_return
  (lambda (expression state continue break throw returnBreak)
    ; if 'return has already been assigned, do not re-assign
    (if (isDeclared 'return state)
        state
        (Mvalue (cdr expression) state (lambda (value newState) (addBinding 'return value newState)) continue break throw returnBreak))))
       
; calulates an expression (mathematical or boolean)
(define compute
  (lambda (expression state return continue break throw returnBreak)
    (cond
      
      ; not
      ((eq? (statementType expression) '!)
       ; determine the value of the left, and only, operand
       (Mvalue (leftOperand expression)
               state
               (lambda (leftVal leftState)
                 ; return the 'not' of the value 
                  (return (not leftVal) leftState))
               continue break throw returnBreak))
      
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
                            (return (+ leftVal rightVal) rightState)) continue break throw returnBreak)) continue break throw returnBreak))
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
                               (return (- leftVal rightVal) rightState)) continue break throw returnBreak))) continue break throw returnBreak))
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
                           (return (* leftVal rightVal) rightState)) continue break throw returnBreak)) continue break throw returnBreak))

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
                           (return (quotient leftVal rightVal) rightState)) continue break throw returnBreak)) continue break throw returnBreak))

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
                           (return (remainder leftVal rightVal) rightState)) continue break throw returnBreak)) continue break throw returnBreak))
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
                           (return (< leftVal rightVal) rightState)) continue break throw returnBreak)) continue break throw returnBreak))

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
                           (return (> leftVal rightVal) rightState)) continue break throw returnBreak)) continue break throw returnBreak))

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
                           (return (<= leftVal rightVal) rightState)) continue break throw returnBreak)) continue break throw returnBreak))

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
                           (return (>= leftVal rightVal) rightState)) continue break throw returnBreak)) continue break throw returnBreak))

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
                                       rightState)) continue break throw returnBreak)) continue break throw returnBreak))

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
                           (return (not (eq? leftVal rightVal)) rightState)) continue break throw returnBreak)) continue break throw returnBreak))

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
                           (return (or leftVal rightVal) rightState)) continue break throw returnBreak)) continue break throw returnBreak))

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
                           (return (and leftVal rightVal) rightState)) continue break throw returnBreak)) continue break throw returnBreak))

      ; otherwise, determine the value of the car of the expression and return
      (else (Mvalue (car expression) state (lambda (value newState) (return value newState)) continue break throw returnBreak))
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
    (cond
      ;((eq? var 'return) (replaceLayer (getNumLayers state) (cons (cons var (varLis (getLayer (getNumLayers state) state))) (cons (cons value (valLis (getLayer (getNumLayers state) state))) '())) state))
    ( (isDeclared var state)
        ; ... can't redeclare
        (error "Variable has already been declared: " var))
        ; otherwise, replace the first layer of the state
        (else (replaceLayer 0
                      ; add binding to the first layer of the state
                      (cons
                       (cons var (varLis (currentLayer state)))
                       (cons (cons value (valLis (currentLayer state))) '()))
                      state)))))
         
; replace an existing binding of a variable
(define replaceBinding
  (lambda (var value state)
    ; get the (layer index) of the variable
    (let ([variableInfo (indexOfVariable var state 0)])
      ; if the layer is -1, variable hasn't been declared
      (if (eq? (car variableInfo) -1)
         (error "Cannot asign variable: variable has not been declared yet:" var)
         ; get the layer that needs to be updated
         (let ([layer (getLayer (car variableInfo) state)])
           ; replace the existing binding 
           (replaceBindingInLayer value (valLis layer) (cadr variableInfo) 0
                                  (lambda (newValLis)
                                    ; update the state & replace the old layer with the new one 
                                    (replaceLayer (car variableInfo)
                                                  (cons (varLis layer)
                                                        (cons newValLis '()))
                                                  state))))))))
      
; replace an existing binding of a variable, at a given index, with the new value
(define replaceBindingInLayer 
  (lambda (value valLis index currIndex return)
    (if (eq? index currIndex)
        (return (cons value (cdr valLis)))
        (replaceBindingInLayer value (cdr valLis) index (+ currIndex 1) (lambda (v) (return (cons (car valLis) v)))))))

; gets the value of a variable given its name 
(define findBindingByName
  (lambda (name state)
    (call/cc
     ; calls helper on the current layer
     (lambda (break) (findBindingByName-helper name (varLis (currentLayer state)) (valLis (currentLayer state))
                                              (lambda (val)
                                                ; if there are no more layers and nothing has been found, error
                                                (if (and (null? (cdr state)) (eq? val -1))
                                                     (error "Variable has not been declared yet: " name)
                                                     ; otherwise recurse on the rest of the state
                                                     (findBindingByName name (cdr state))))
                                              break)))))
                                                    
; helper method that splits the state to recursively search
(define findBindingByName-helper
 (lambda (name varLis valLis return break)
    (cond
      ; if either list is empty, variable has not been declared- throw error 
      ((or (null? varLis) (null? valLis)) (return -1))
      ; if the name is found, return coresponding value
      ((eq? name (car varLis)) (break (parseValue (car valLis))))
      ; otherwise keep searching
      (else                    (findBindingByName-helper name (cdr varLis) (cdr valLis) return break)))))

; returns a proper value or error
(define parseValue
  (lambda (value)
    (cond
      ((eq? value 'NULL)     (error "Variable has not been assigned"))
      ((eq? value #t)        'true)
      ((eq? value #f)        'false)
      (else                  value))))
                             
; ------Abstractions--------------------

; Add to the top layer
(define getNumLayers
  (lambda (state)
    (getNumLayers-helper 0 state (lambda (v) v))))

(define getNumLayers-helper
  (lambda (currNum state return)
    (if (null? (cdr state))
        (return currNum)
        (getNumLayers-helper (+ currNum 1) (cdr state) return))))

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
    ; if the layer has been found 
    (if (eq? layer currLayer)
        ; return the layer
        (return (car state))
        ; otherwise, keep searching 
        (getLayer-helper layer (+ 1 currLayer) (cdr state) return))))

; Replaces the specified layer
(define replaceLayer
  (lambda (layer newLayer state)
    (replaceLayer-helper layer 0 state newLayer (lambda (v) v))))
    
(define replaceLayer-helper
  (lambda (layer currLayer state newLayer return)
    ; if the layer to be replaced has been found 
    (if (eq? layer currLayer)
        ; replace the layer
        (return (cons newLayer (cdr state)))
        ; otherwise, recurse on the remainder of the state
        (replaceLayer-helper layer (+ 1 currLayer) (cdr state) newLayer
                             ; and return the cons of the first part of the state with the result
                             (lambda (restOfState) (return (cons (car state) restOfState)))))))
    
    
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