#lang racket

(require rackunit
         rackunit/text-ui
         "lang-4-11.rkt")
(define-test-suite pruebas
  (test-case "list"
             (check-equal? (run `(list ((- 5 3)
                                        (let (x (newref 3))
                                          (deref x)))))
                           (list (num-val 2) (num-val 3)))

             (check-equal? (run `(list ((- 5 3)
                                        (zero? 6))))
                           (list (num-val 2) (bool-val #f)))

             (check-equal? (run `(list ((- 5 3)
                                        (zero? 0)
                                        (let (x (newref 3))
                                          (deref x)))))
                           (list (num-val 2) (bool-val #t) (num-val 3)))))

(run-tests pruebas 'verbose)