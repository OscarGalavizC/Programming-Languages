#lang plait
	
(define-type ArithC
   (numC [n : Number])
   (plusC [l : ArithC] [r : ArithC])
   (multC [l : ArithC] [r : ArithC]))


(define-type ArithS
  (numS [n : Number])
  (plusS [l : ArithS] [r : ArithS])
  (multS [l : ArithS] [r : ArithS])
  (bminus [l : ArithS] [r : ArithS])
  (uminus [l : ArithS]))

 (define (parse [s : S-Exp]) : ArithS
   (cond [(s-exp-number? s) (numS (s-exp->number s))]
         [(s-exp-list? s)
          (let ([ls (s-exp->list s)])
            (cond
              [(eq? (length ls) 3)
               (case (s-exp->symbol (first ls))
                 [(+) (plusS (parse (second ls)) (parse (third ls)))]
                 [(*) (multS (parse (second ls)) (parse (third ls)))]
                 [(-) (bminus (parse (second ls)) (parse (third ls)))]
                 [else (error 'parse "expresión aritmética malformada")])]
              [(eq? (length ls) 2)
               (case (s-exp->symbol (first ls))
                 [(-) (uminus (parse (second ls)))]
                 [else (error 'parse "expresión aritmética malformada")])]
              [else (error 'parse "expresión aritmética malformada")]))]
         [else (error 'parse "expresión aritmética malformada")]))

(define (desugar [s : ArithS]) : ArithC
  (type-case ArithS s
    [(numS n) (numC n)]
    [(plusS l r) (plusC (desugar l) (desugar r))]
    [(multS l r) (multC (desugar l) (desugar r))]
    [(bminus l r) (plusC (desugar l) (multC (numC -1) (desugar r)))]
    [(uminus l) (multC (numC -1) (desugar l))]))

(define (interp [s : ArithC]) : Number
  (type-case ArithC s
    [(numC n) n]
    [(plusC l r) (+ (interp l) (interp r))]
    [(multC l r) (* (interp l) (interp r))]))

(define (eval [s : S-Exp]) : Number
  (interp (desugar (parse s))))