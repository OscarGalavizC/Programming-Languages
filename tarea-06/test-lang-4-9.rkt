#lang racket

(require rackunit
         rackunit/text-ui
         "lang-4-9.rkt")
(define-test-suite pruebas
  (test-case "allofit"
             (check-eqv? (deref (newref 4)) 4)

             (let ([ref1 (newref 5)]
                   [ref2 (newref 6)]
                   [ref3 (newref 15)])
               (check-eqv? (deref ref1) 5)
               (check-eqv? (deref ref2) 6)
               (check-eqv? (deref ref3) 15)
               (setref! ref2 34)
               (check-eqv? (deref ref1) 5)
               (check-eqv? (deref ref2) 34)
               (check-eqv? (deref ref3) 15))))

(run-tests pruebas 'verbose)