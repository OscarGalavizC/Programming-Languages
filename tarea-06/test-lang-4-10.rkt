#lang racket

(require rackunit
         rackunit/text-ui
         "lang-4-10.rkt")
(define-test-suite pruebas
  (test-case "begin"
             (check-equal? (run `(begin (1 4 6 1)))
                           (num-val 1))

             (check-equal? (run `(let (x (newref 5))
                                   (begin ((setref x 8)
                                          (- (deref x) 2)))))
                           (num-val 6))))

(run-tests pruebas 'verbose)