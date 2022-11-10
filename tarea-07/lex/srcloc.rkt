#lang racket/base

(require "position.rkt")

(port-count-lines-enabled #t)

(define reopen-source (make-parameter #f))

(define (location-message src beg end)
  (srcloc->string
   (srcloc (object-name src)
           (pos-line beg)
           (pos-col beg)
           (pos-offset beg)
           (- (pos-offset end) (pos-offset beg)))))

(define (source-offset src)
  (define-values (line col offset)
    (port-next-location src))
  offset)

(define (maybe-highlight src beg-off end-off)
  (define line1 (read-line src))
  (define offset1 (source-offset src))
  (cond
    [(eof-object? line1) ""]
    [(<= beg-off offset1)
     (define prefix (max 0 (sub1 beg-off)))
     (define span (min (max (- end-off beg-off 1) 0)
                       (- (string-length line1) prefix 1)))
     (string-append
      (format "~n~a~n" line1)
      (make-string prefix #\space)
      "^"
      (make-string span #\-)
      "\n")]
    [else
     (define line2 (read-line src))
     (define offset2 (source-offset src))
     (do-highlight src beg-off end-off line1 offset1 line2 offset2)]))

(define (do-highlight src beg-off end-off line1 offset1 line2 offset2)
  (cond
    [(eof-object? line2) ""]
    [(<= beg-off offset2)
     (define prefix (max 0 (- beg-off offset1)))
     (define span (min (max (- end-off beg-off 1) 0)
                       (- (string-length line2) prefix 1)))
     (string-append
      (format "~n~a~n~a~n" line1 line2)
      (make-string prefix #\space)
      "^"
      (make-string span #\-)
      "\n")]
    [else
     (define line3 (read-line src))
     (define offset3 (source-offset src))
     (do-highlight src beg-off end-off line2 offset2 line3 offset3)]))

(define (error/context msg beg end)
  (if (not (reopen-source))
      (error 'imp "~a" msg)
      ((reopen-source)
       (lambda (src)
         (error 'imp "~a: ~a~a"
                (location-message src beg end)
                msg
                (maybe-highlight src (pos-offset beg) (pos-offset end)))))))

(provide reopen-source
         error/context)
