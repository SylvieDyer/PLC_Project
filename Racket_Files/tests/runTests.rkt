#lang racket
(require "../LanguageInterpreterSolution.rkt")

; (runTest 1) to run tests corresponding to proj 1
; (runTest 2) to run tests corresponding to proj 2
; NOTE: This does NOT run tests meant to produce errors
; those will have to be manually tested (can find these in the "Error Tests" subfolder then run in shell (interpret "/ErrorTests/tx.txt")
(define runTest
  (lambda (v)
    ;(let ([tests '("t1.txt" "t2.txt" "t3.txt" "t4.txt" "t5.txt")])

    (runTest-helper '(
                     ; ("3t1.txt" 10) ("3t2.txt" 14) ("3t3.txt" 45)
                     ;                 ("3t4.txt" 55)
                     ;                 ("3t5.txt" 1) ("3t6.txt" 115) ("3t7.txt" true) ("3t8.txt" 20) ("3t9.txt" 24)
                     ;                   ("3t10.txt" 2) ("3t11.txt" 35) ("3t13.txt" 90) ("3t14.txt" 69) ("3t15.txt" 87) ("3t16.txt" 64) ("3t18.txt" 125)
                     ;                   ("3t19.txt" 100) ("3t20.txt" 2000400)
                     ;                   ("3t21.txt" 3421)
                     ;                   ("3t22.txt" 20332) ("3t23.txt" 21)))
                 ;     ("4t1.txt" 15) ("4t2.txt" 12) ("4t3.txt" 125) (
)
;    (if (eq? v 1)
;        (runTest-helper '( ("t1.txt" 150) ("t2.txt" -4) ("t3.txt" 10) ("t4.txt" 16) ("t5.txt" 220) ("t6.txt" 5) ("t7.txt" 6) ("t8.txt" 10) ("t9.txt" 5)
;                                         ("t10.txt" -39) ("t15.txt" true) ("t16.txt" 100) ("t17.txt" false) ("t18.txt" true) ("t19.txt" 128)
;                                        ("t20.txt" 12) ("t21.txt" 30) ("t22.txt" 11) ("t23.txt" 1106) ("t24.txt" 12) ("t25.txt" 16) ("t26.txt" 72)
;                                         ("t27.txt" 21) ("t28.txt" 164)))
;        (runTest-helper '( ("2t1.txt" 20) ("2t2.txt" 164) ("2t3.txt" 32) ("2t4.txt" 2) ("2t6.txt" 25) ("2t7.txt" 21) ("2t8.txt" 6) ("2t9.txt" -1)
;                                         ("2t10.txt" 789) ("2t15.txt" 125) ("2t16.txt" 110) 
;                                         ("2t18.txt" 101) ("2t20.txt" 21)
;
;                                         ("2t17.txt" 20004000)

                                         ))

(define runTest-helper
  (lambda (tests)
    (print (car tests))
    (if (eq? (interpret (caar tests)) (cadar tests))
        (println "Warm Fuzzies ")
        (begin (print "Test Failed :( Incorrect output given is:") (println (interpret (caar tests)))))
    (if (null? (cdr tests))
        (println "Tests Done!")
        (runTest-helper (cdr tests)))))

(runTest 0)