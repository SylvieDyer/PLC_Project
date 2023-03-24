#lang racket
(require "../LanguageInterpreterProject.rkt")

; (runTest 1) to run tests corresponding to proj 1
; (runTest 2) to run tests corresponding to proj 2
; NOTE: This does NOT run tests meant to produce errors
; those will have to be manually tested (can find these in the "Error Tests" subfolder then run in shell (interpret "/ErrorTests/tx.txt")
(define runTest
  (lambda (v)
    ;(let ([tests '("t1.txt" "t2.txt" "t3.txt" "t4.txt" "t5.txt")])
    (if (eq? v 1)
        (runTest-helper '( ("t1.txt" 150) ("t2.txt" -4) ("t3.txt" 10) ("t4.txt" 16) ("t5.txt" 220) ("t6.txt" 5) ("t7.txt" 6) ("t8.txt" 10) ("t9.txt" 5)
                                         ("t10.txt" -39) ("t15.txt" true) ("t16.txt" 100) ("t17.txt" false) ("t18.txt" true) ("t19.txt" 128)
                                         ("t20.txt" 12) ("t21.txt" 30) ("t22.txt" 11) ("t23.txt" 1106) ("t24.txt" 12) ("t25.txt" 16) ("t26.txt" 72)
                                         ("t27.txt" 21) ("t28.txt" 164)))
        (runTest-helper '( ("2t1.txt" 20) ("2t2.txt" 164) ("2t3.txt" 32) ("2t4.txt" 2) ("2t6.txt" 25) ("2t7.txt" 21) ("2t8.txt" 6) ("2t9.txt" -1)
                                         ("2t10.txt" 789) ("2t15.txt" 125) ("2t16.txt" 110) ("2t17.txt" 20004000) ("2t18.txt" 101) ("2t20.txt" 21) )))))

(define runTest-helper
  (lambda (tests)
    (print (car tests))
    (if (eq? (interpret (caar tests)) (cadar tests))
        (println "Warm Fuzzies ")
        (begin (print "Test Failed :( Incorrect output given is:") (println (interpret (caar tests)))))
    (if (null? (cdr tests))
        (println "Tests Done!")
        (runTest-helper (cdr tests)))))

(runTest 1)
(runTest 2)