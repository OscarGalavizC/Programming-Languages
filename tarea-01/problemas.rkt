#lang racket

;; Escribe aquí tus soluciones

;;1
(define (countdown n)
  (if (equal? n 0)
      (list 0)
      (cons n (countdown (- n 1)))))

;;2
(define (insertL a b ls)
  (if (empty? ls)
      '()
      (if (eq? a (first ls))
          (cons b (cons (first ls) (insertL a b (rest ls))))
          (cons (first ls) (insertL a b (rest ls))))))

;;3
(define (remv-1st a ls)
  (if (empty? ls)
      ls
      (if (eq? a (first ls))
          (rest ls)
          (cons (first ls) (remv-1st a (rest ls))))))

;;4
(define (map p ls)
  (if (empty? ls)
      ls
      (cons (p (first ls)) (map p (rest ls)))))

;;5
(define (filter p ls)
  (if (empty? ls)
      '()
      (if (p (first ls))
          (cons (first ls) (filter p (rest ls)))
          (filter p (rest ls)))))

;;6
(define (zip ls1 ls2)
  (if (eq? #t (or (empty? ls1) (empty? ls2)))
      '()
      (cons (cons (first ls1) (first ls2)) (zip (rest ls1) (rest ls2)))))

;;7
(define (list-index-ofv a ls)
  (if (empty? ls)
      -1
      (if (eq? a (first ls))
          0
          (+ (list-index-ofv a (rest ls)) 1))))

;;8
(define (append ls1 ls2)
  (if (empty? ls1)
      ls2
      (cons (first ls1) (append (rest ls1) ls2))))

;;9
(define (reverse ls)
  (if (empty? ls)
      ls
      (if (empty? (rest ls))
          ls
          (append (reverse (rest ls)) (list (first ls))))))

;;10
(define (repeat ls n)
  (if (eq? n 0)
      '()
      (append ls (repeat ls (- n 1)))))

;;11
(define (same-lists* ls1 ls2)
  (equal? ls1 ls2))

;;12


;;13
(define (binary->natural ls)
  (if (empty? ls)
      0
      (if (eq? #f (or (eq? (first ls) 0) (eq? (first ls) 1)))
          (error "no es numero binario")
          (+ (first ls) (* 2 (binary->natural (rest ls)))))))

;;14
(define (div a b)
  (cond
    [(eq? b 0) (error "no se puede dividir entre 0")]
    [(eq? a 0) 0]
    [(integer? (/ a b)) (+ 1 (div (- a b) b))]
    [else (error "division no exacta")]))

;;15
(define (append-map p ls)
  (if (empty? ls)
      ls
      (append (p (first ls)) (append-map p (rest ls)))))

;;16
(define (set-difference ls1 ls2)
  (if (empty? ls2)
      ls1
      (set-difference (remv-1st (first ls2) ls1) (rest ls2))))

;;17
(define (foldr op ac ls)
  (if (empty? ls)
      ac
      (op (first ls) (foldr op ac (rest ls)))))
      

;;18
(define (powerset ls)
  (if (empty? ls)
      (list ls)
      (append (map (lambda (n) (cons (first ls) n)) (powerset (rest ls))) (powerset (rest ls)))))

;;19
(define (cartesian-product ls)
  (if (or (empty? (first ls)) (empty? (first (rest ls))))
      null
      (append (map (lambda (n) (list (first (first ls)) n)) (first (rest ls)))
              (cartesian-product (list (rest (first ls)) (first (rest ls)))))))

;;20
(define (insertL-fr a b ls)
  (foldr (lambda (x y) (if (eq? x a)
                           (cons b (cons a y))
                           (cons x y)))
         null
         ls))

(define (filter-fr p ls)
  (foldr (lambda (x y) (if (p x)
                           (cons x y)
                           y))
         null
         ls))
                           
(define (map-fr p ls)
  (foldr (lambda (x y) (cons (p x) y))
         null
         ls))

(define (append-fr ls1 ls2)
  (foldr (lambda (x y) (cons x y))
         ls2
         ls1))

(define (reverse-fr ls)
  (foldr (lambda (x y) (append y (list x)))
         null
         ls))

(define (binary->natural-fr ls)
  (foldr (lambda (x y) (+ x (* 2 y)))
         0
         ls))

(define (append-map-fr p ls)
  (foldr (lambda (x y) (append (p x) y))
         null
         ls))

(define (set-difference-fr ls1 ls2)
  (foldr (lambda (x y) (remv-1st x y))
         ls1
         ls2))

(define (powerset-fr ls)
  (foldr (lambda (x y) (cons (list x) y))
         '(())
         ls))

(provide (all-defined-out))
