#lang racket

;; 1.
(define pi 3.14)

;; 2.
(define (area-circle r)
  (if (eq? r null)
      (append r 0)
      (* pi r r)))

;; 3.
(define (circle-properties r)
  (if (eq? r null)
      (list (* pi 0 0)(* 2 pi 0))
      (list (* pi r r)(* 2 pi (abs r)))))

;; 4.
(define (rectangle-properties rec)
  (if (null? rec)
      (append rec (list 0 0))
      (list (* (abs (list-ref rec 0)) (abs (list-ref rec 1)))
            (* 2 (+ (abs (list-ref rec 1)) (abs (list-ref rec 0)))))
      ))

;; 5.
(define (find-needle ls)
  (if (null? ls)
      -1
      (if (eq? (list-ref ls 0) 'needle)
          0
          (if (eq? (list-ref ls 1) 'needle)
              1
              (if (eq? (list-ref ls 2) 'needle)
                  2
                  -1)))))

 6.
(define (abs x)
  ...)

;; 7.
;;(define (inclis1 ls)
;;  ...)

;; 8.
;;(define (even? x)
;;  ...)

;; 9.
;;(define another-add
;;  (lambda (n m)
;;    ...))
(provide (all-defined-out))
