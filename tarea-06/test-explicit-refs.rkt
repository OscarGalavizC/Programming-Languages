#lang racket

(require rackunit
         rackunit/text-ui
         "explicit-refs.rkt")

(define-test-suite pruebas
  
  (test-case "const"
             (check-equal? (run `5)
                           (eval (num-val 5) '()))
             (check-equal? (run `23)
                           (eval (num-val 23) '())))

  (test-case "diff"
             (check-equal? (run `(- 3 4))
                           (eval (num-val -1) '()))
             (check-equal? (run `(- 4 (- 2 3)))
                           (eval (num-val 5) '()))
             (check-equal? (run `(- (- 9 4) (- 2 5)))
                           (eval (num-val 8) '())))

  (test-case "zero?"
             (check-equal? (run `(zero? 0))
                           (eval (bool-val #t) '()))
             (check-equal? (run `(zero? 7))
                           (eval (bool-val #f) '()))
             (check-equal? (run `(zero? (- 11 11)))
                           (eval (bool-val #t) '()))
             (check-equal? (run `(zero? (- 11 10)))
                           (eval (bool-val #f) '())))

  (test-case "if"
             (check-equal? (run `(if (zero? 0) 5 7))
                           (eval (num-val 5) '()))
             (check-equal? (run `(if (zero? 7) 3 4))
                           (eval (num-val 4) '())))
  
  (test-case "let"
             (check-equal? (run `(let (x 5) 4))
                           (eval (num-val 4) '()))
             (check-equal? (run `(let (x 7) x))
                           (eval (num-val 7) '()))
             (check-equal? (run `(let (x 4) (- x 1)))
                           (eval (num-val 3) '()))
             (check-equal? (run `(let (x 4)
                                   (let (y 3)
                                     (let (z 2)
                                       (- x (- y z))))))
                           (eval (num-val 3) '())))

  (test-case "proc/call"
             (check-equal? (run `(let (f (proc x x)) (f 5)))
                           (eval (num-val 5) '()))
             (check-equal? (run `(let (g (proc y (- y (- 0 1)))) (g 6)))
                           (eval (num-val 7) '()))
             (check-equal? (run `(let (h (proc z (zero? z))) (h 6)))
                           (eval (bool-val #f) '()))
             (check-equal? (run `(let (h (proc z (zero? z))) (h 0)))
                           (eval (bool-val #t) '())))

  (test-case "begin"
             (check-equal? (run `(begin (1 4 6 1)))
                           (num-val 1))

             (check-equal? (run `(let (x (newref 5))
                                   (begin ((setref x 8)
                                           (- (deref x) 2)))))
                           (num-val 6)))

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
                           (list (num-val 2) (bool-val #t) (num-val 3))))

  (test-case "newref/deref/setref"
             (check-equal? (run `(let (x (newref 6)) (deref x)))
                           (eval (num-val 6)
                                 (list '() (num-val 6))))
             (check-equal? (run `(let (x (newref 6))
                                   (let (y (newref 8))
                                     (- (deref y) (deref x)))))
                           (eval (num-val 2)
                                 (list (list '() (num-val 6)) (num-val 8))))))


(run-tests pruebas 'verbose)