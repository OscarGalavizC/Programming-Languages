#lang racket

(require rackunit
         rackunit/text-ui
         "problemas.rkt")

(define-test-suite pruebas

  ;;Problema 2
  ;;Problema 4
  (test-case "bundle"
    (check-equal? (bundle '("a" "b" "c") 3)
                  '("abc"))
    (check-equal? (bundle '("a" "b" "c" "d" "e" "f") 3)
                  '("abc" "def"))
    (check-equal? (bundle '("a" "b" "c" "d" "e" "f" "g") 3)
                  '("abc" "def" "g"))
    (check-equal? (bundle '("a" "b") 3)
                  '("ab"))
    (check-equal? (bundle '() 3)
                  '())
    (check-equal? (bundle '("") 3)
                  '(("")))
    (check-equal? (bundle '(1 2 3) 3)
                  '((1 2 3)))
    (check-equal? (bundle '(1 2 3 4 5 6) 3)
                  '((1 2 3) (4 5 6)))
    (check-equal? (bundle '(1 2 3 4 5 6 7) 3)
                  '((1 2 3) (4 5 6) (7))))

  (test-case "take"
    (check-equal? (take '(1 2 3 4 5 6) 3)
                  '(1 2 3))
    (check-equal? (take '(1 2 3 4 5 6) 9)
                  '(1 2 3 4 5 6))
    (check-equal? (take '(1 2 3 4 5 6) 0)
                  '())
    (check-equal? (take '("a" 2 "c" 4 "d" 6) 3)
                  '("a" 2 "c"))
    (check-equal? (take '() 9)
                  '()))

  (test-case "drop"
    (check-equal? (drop '(1 2 3 4 5 6) 3)
                  '(4 5 6))
    (check-equal? (drop '(1 2 3 4 5 6) 9)
                  '())
    (check-equal? (drop '(1 2 3 4 5 6) 0)
                  '(1 2 3 4 5 6))
    (check-equal? (drop '("a" 2 "c" 4 "d" 6) 3)
                  '(4 "d" 6))
    (check-equal? (drop '() 9)
                  '()))

  (test-case "list->chunks"
    (check-equal? (list->chunks '(1 2 3) 3)
                  '((1 2 3)))
    (check-equal? (list->chunks '(1 2 3 4 5 6) 3)
                  '((1 2 3) (4 5 6)))
    (check-equal? (list->chunks '(1 2 3 4 5 6 7) 3)
                  '((1 2 3) (4 5 6) (7)))
    (check-equal? (list->chunks '() 3)
                  '())
    (check-equal? (list->chunks '(1 2) 3)
                  '((1 2)))
    (check-equal? (list->chunks '(1 2 3 4 5 6 7) 1)
                  '((1) (2) (3) (4) (5) (6) (7))))

  (test-case "partition"
    (check-equal? (partition "abc" 3)
                  '("abc"))
    (check-equal? (partition "abcdef" 3)
                  '("abc" "def"))
    (check-equal? (partition "abcdefg" 3)
                  '("abc" "def" "g"))
    (check-equal? (partition "ab" 3)
                  '("ab")))

  (test-case "isort"
    (check-equal? (isort '(2 1 1 3) #t)
                  '(1 1 2 3))
    (check-equal? (isort '(2 1 1 3) #f)
                  '(3 2 1 1))
    (check-equal? (isort '() #f)
                  '())
    (check-equal? (isort '() #t)
                  '())
    (check-equal? (isort '(0 9 5 1 5 2 6 7 8) #t)
                  '(0 1 2 5 5 6 7 8 9))
    (check-equal? (isort '(0 9 5 1 5 2 6 7 8) #f)
                  '(9 8 7 6 5 5 2 1 0)))

  (test-case "insert"
    (check-equal? (insert 0 '(1 6 9 5 3) #t)
                  '(0 1 6 9 5 3))
    (check-equal? (insert 0 '(1 6 9 5 3) #f)
                  '(1 6 9 5 3 0))
    (check-equal? (insert 10 '(1 6 9 5 3) #t)
                  '(1 6 9 5 3 10))
    (check-equal? (insert 10 '(1 6 9 5 3) #f)
                  '(10 1 6 9 5 3))
    (check-equal? (insert 0 '() #t)
                  '(0))
    (check-equal? (insert 0 '() #f)
                  '(0)))

  (test-case "smallers"
    (check-equal? (smallers '(1 2 3 4 5 6 7) 5)
                  '(1 2 3 4))
    (check-equal? (smallers '(1 2 3 4 5 6 7) 1)
                  '())
    (check-equal? (smallers '(1 2 3 4 5 6 7) 20)
                  '(1 2 3 4 5 6 7))
    (check-equal? (smallers '() 5)
                  '()))

  (test-case "largers"
    (check-equal? (largers '(1 2 3 4 5 6 7) 5)
                  '(6 7))
    (check-equal? (largers '(1 2 3 4 5 6 7) 1)
                  '(2 3 4 5 6 7))
    (check-equal? (largers '(1 2 3 4 5 6 7) 20)
                  '())
    (check-equal? (largers '() 5)
                  '()))

  (test-case "equallers"
             (check-equal? (equallers '(1 2 3 4 5 6 7) 5)
                           '(5))
             (check-equal? (equallers '(1 2 3 4 6 7) 5)
                           '())
             (check-equal? (equallers '(1 2 3 4 5 6 7 1 7 4) 1)
                           '(1 1))
             (check-equal? (equallers '(1 2 20 3 4 5 20 6 20 7) 20)
                           '(20 20 20))
             (check-equal? (equallers '() 5)
                           '()))
  
  (test-case "quicksortfix"
    (check-equal? (quicksortfix '(2 1 1 3))
                  '(1 1 2 3))
    (check-equal? (quicksortfix '())
                  '())
    (check-equal? (quicksortfix '(0 9 5 1 5 2 6 7 8))
                  '(0 1 2 5 5 6 7 8 9)))
  
  (test-case "smallers2"
    (check-equal? (smallers2 '("a" "b" "d" "e" "f" "g") "c")
                  '("a" "b"))
    (check-equal? (smallers2 '("avion" "botella" "dedo" "elefante" "focote" "gaton") "c")
                  '("avion" "botella"))
    (check-equal? (smallers2 '() "c")
                  '()))

  (test-case "largers2"
    (check-equal? (largers2 '("a" "b" "d" "e" "f" "g") "c")
                  '("d" "e" "f" "g"))
    (check-equal? (largers2 '("avion" "botella" "dedo" "elefante" "focote" "gaton") "c")
                  '("dedo" "elefante" "focote" "gaton"))
    (check-equal? (largers2 '() "c")
                  '()))

  (test-case "equallers2"
    (check-equal? (equallers2 '("c" "a" "b" "d" "c" "e" "f" "g" "c") "c")
                  '("c" "c" "c"))
    (check-equal? (equallers2 '("a" "b" "d" "e" "f" "g") "c")
                  '())
    (check-equal? (equallers2 '("carrito" "avion" "botella" "carrito" "dedo" "elefante" "focote" "gaton" "carrito") "carrito")
                  '("carrito" "carrito" "carrito"))
    (check-equal? (equallers2 '("carrito" "avion" "botella" "carrito" "dedo" "elefante" "focote" "gaton" "carrito") "c")
                  '("carrito" "carrito" "carrito"))
    (check-equal? (equallers2 '("avion" "botella" "dedo" "elefante" "focote" "gaton") "carrito")
                  '())
    (check-equal? (equallers2 '("avion" "botella" "dedo" "elefante" "focote" "gaton") "c")
                  '())
    (check-equal? (equallers2 '() "c")
                  '()))

  (test-case "quicksort2"
    (check-equal? (quicksort2 '(2 1 1 3) >)
                  '(1 1 2 3))
    (check-equal? (quicksort2 '(2 1 1 3) <)
                  '(3 2 1 1))
    (check-equal? (quicksort2 '() <)
                  '())
    (check-equal? (quicksort2 '() >)
                  '())
    (check-equal? (quicksort2 '(0 9 5 1 5 2 6 7 8) >)
                  '(0 1 2 5 5 6 7 8 9))
    (check-equal? (quicksort2 '(0 9 5 1 5 2 6 7 8) <)
                  '(9 8 7 6 5 5 2 1 0))
    (check-equal? (quicksort2 '("cero" "nueve" "cinco" "uno" "tres" "dos" "seis" "siete" "ocho") string>?)
                  '("cero" "uno" "dos" "cinco" "cinco" "seis" "siete" "ocho" "nueve")))

  (test-case "quicksort3"
    (check-equal? (quicksort3 '(2 1 1 3))
                  '(1 1 2 3))
    (check-equal? (quicksort3 '())
                  '())
    (check-equal? (quicksort3 '(0 9 5 1 5 2 6 7 8))
                  '(0 1 2 5 5 6 7 8 9)))

  (test-case "smallers3"
             (check-equal? (smallers3 '(1 2 3 4 5 6 7) 5)
                           '(1 2 3 4))
             (check-equal? (smallers3 '(1 2 3 4 5 6 7) 1)
                           '())
             (check-equal? (smallers3 '(1 2 3 4 5 6 7) 20)
                           '(1 2 3 4 5 6 7))
             (check-equal? (smallers3 '() 5)
                           '()))

  (test-case "largers3"
    (check-equal? (largers3 '(1 2 3 4 5 6 7) 5)
                  '(6 7))
    (check-equal? (largers3 '(1 2 3 4 5 6 7) 1)
                  '(2 3 4 5 6 7))
    (check-equal? (largers3 '(1 2 3 4 5 6 7) 20)
                  '())
    (check-equal? (largers3 '() 5)
                  '()))

  (test-case "quicksort4"
    (check-equal? (quicksort4 '(2 1 1 3))
                  '(1 1 2 3))
    (check-equal? (quicksort4 '())
                  '())
    (check-equal? (quicksort4 '(0 9 5 1 5 2 6 7 8))
                  '(0 1 2 5 5 6 7 8 9)))
  
  )
(run-tests pruebas 'verbose)