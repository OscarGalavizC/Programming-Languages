#lang racket

(define (empty-store)
  (make-vector 0))

(define the-store 'uninitialized)

(define (get-store)
  the-store)

(define (initialize-store!)
  (set! the-store (empty-store)))

(define (reference? v)
  (integer? v))

(define (extend-store store val)
  (let* ([store-size (vector-length store)]
         [new-store (make-vector (+ store-size 1))])
    (vector-set! new-store store-size val)
    (vector-copy! new-store 0 store 0)
    (cons new-store store-size)))

(define (newref val)
  (let* ([new-store-info (extend-store the-store val)]
        [new-store (car new-store-info)]
        [new-ref (cdr new-store-info)])
    (set! the-store new-store)
    new-ref))

(define (deref ref)
  (vector-ref the-store ref))

(define (setref! ref val)
  (if (and (reference? ref) (< ref (vector-length the-store)))
      (vector-set! the-store ref val)
      (#f)))
(initialize-store!)
(provide (all-defined-out))