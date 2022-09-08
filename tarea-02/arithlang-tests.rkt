#lang plait

(require "arithlang.rkt")

;Num Tests
(test (eval `8) 8)

(test (eval `-9) -9)

(test (eval `0) 0)

;Plus Tests
(test (eval `(+ 1 2)) 3)

(test (eval `(+ 6 -4)) 2)

(test (eval `(+ 6 (+ 3 -2))) 7)

(test (eval `(+ 6 (* 3 -2))) 0)

(test (eval `(+ 6 (- 8 5))) 9)

(test (eval `(+ 6 (- 8))) -2)

;Mult Tests
(test (eval `(* 6 -4)) -24)

(test (eval `(* 100 0)) 0)

(test (eval `(* 6 (+ 3 5))) 48)

(test (eval `(* 6 (* 3 5))) 90)

(test (eval `(* 6 (- 3 5))) -12)

(test (eval `(* 6 (- 3))) -18)

;Bminus Tests

(test (eval `(- 4 8)) -4)

(test (eval `(- 4 8)) -4)

(test (eval `(- 4 (+ 12 4))) -12)

(test (eval `(- (+ 12 4) 8)) 8)

(test (eval `(- 4 (* 2 3))) -2)

(test (eval `(- (+ 3 5) (* 2 3))) 2)

;Uminus Tests

(test (eval `(- 5)) -5)

(test (eval `(- 0)) 0)

(test (eval `(- (* 2 3))) -6)

(test (eval `(- (+ (* 2 3) (- 9 3)))) -12)