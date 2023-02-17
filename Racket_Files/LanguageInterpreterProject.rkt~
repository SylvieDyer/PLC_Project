#lang racket
(require "simpleParser.rkt")

(define interpret
  (lambda (filename)
    (evaluate (parser filename) '(() ()) (lambda (v) v))))


(define evaluate
  (lambda (tree state return)
    (cond
      ((null? tree) state)

      ; entering sub-tree 
      ((list? (car tree)) (evaluate (car tree)
                                    state
                                    (lambda (v)
                                      (return (evaluate (cdr tree) v return)))))
      
      ; declaring variable
      ((eq? (car tree) 'var) (return (Mstate_var tree state)))
      
       ; if there is a value past the variable name
       ;(if (pair? (cddr tree))
          ; (return (declareVariableValue (cadr tree) (caddr tree) state))
           ; (return (cons (cons (cadr tree) (cons (caddr tree) '())) state))
           ; (return (cons (cons (cadr tree) '()) state))))
          ; (return (declareVariable (cadr tree) state))))

      
      ; assigning variable
      ((eq? (car tree) '=)  (return(Mstate_assign (cddr tree) state)))
= y 20
      )
      ; entering if statement

      ; entering while statement
      
  )))

(define Mvalue
  (lambda (expression state return)
    (cond
      ((null? expression) 0)
      ((list? (car expresion)) (Mvalue (car expression) state (lambda (v)
                                                                (Mcalc
                                                                 (car expression)
                                                                 (Mvalue (cdr expression) (Mstate(car expression state (lambda (a) a))) return)
                                                                  v))))


(define Mstate_var
  (lambda (expression state)
    (if (pair? (cddr expression))
        (addBinding (cadr expression)  (evalExpr (caddr expression)) state)
        (addBinding (cadr expression) 'NULL state))))
        ;(cons (cons (cadr expression) (cons (caddr expression) '())) state)
        ;(cons (cons (cadr expression) '()) state))))

; bindings are only added when declaring, so no need for checks!
(define addBinding
  (lambda (var value state)
    (cons (cons var (car state))
          (cons (cons value (cadr state)) '()))))
     

(define Mstate_assign
 (lambda (expression state)
   (cons (car state)
         (cons (replaceBinding (caddr expression)
                               (cadr state)
                               (isDeclared (cadr expression) (car state) (lambda (v) v))
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

(define replaceBinding
  (lambda (value valLis index currIndex return)
    (if (eq? index currIndex)
        (return (cons value (cdr valLis)))
        (replaceBinding value (cdr valLis) index (+ currIndex 1) (lambda (v) (return (cons (car valLis) v)))))))



(define declareVariable
  (lambda (variable state)
     (cons (cons variable '()) state)
    ))

(define declareVariableValue
  (lambda (variable value state)
    (cons (cons variable (cons value '())) state)
    ))
  
;(define assignVariable
;  (lambda (variable value state)
    
  ;  ))
    