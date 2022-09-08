#lang racket

(define (unit-string? x)
  (and (string? x)
      (= (string-length x) 1)))

(define(unit-string-list? x)
  (or (null? x)
     (and (pair? x)
         (string? (first x))
         (= (string-length (first x)) 1)
         (unit-string-list? (rest x)))))

(define (explode s)
  (unless (string? s)
    (error 'explode "esperaba una cadena, pero recibí: ~e" s))
  (map string (string->list s)))

(define (implode ls)
  (unless (unit-string-list? ls)
    (error 'implode "esperaba una lista de cadenas unitarias, pero recibí: ~e" ls))
  (apply string-append ls))

;;Problema 3
;;take : (listof number?) integer? -> (listof number?)
;;take toma una lista l y regresa los primeros n elementos.
;;Construye una lista con el primer elemento de esta y usa recursivamente take con el resto de la lista y (n-1) hasta que n
;;llegue a 0.
(define (take l n)
  (cond
    [(>= n (length l)) l]
    [(= n 0) '()]
    [else (cons (first l) (take (rest l) (- n 1)))]))

;;Problema 3
;;drop : (listof number?) integer? -> (listof number?)
;;drop toma una lista l y regresa la lista sin los primeros n elementos
;;Ignora los primeros n elementos hasta que recursivamente n llega a 0, una vez ahí construye una lista con el primer elemento
;;de la lista y usa recursivamente drop con el resto de la lista.
(define (drop l n)
  (cond
    [(>= n (length l)) '()]
    [(= n 0) (cons (first l) (drop (rest l) n))]
    [else (drop (rest l) (- n 1))]))

;;Problema 6
;;list->chunks : (listof number?) integer? -> (listof (listof number?))
;;list->chunks recibe una lista de números s y un natural n, regresa una lista de sublistas de tamaño n
;;Construye con la lista de los primeros n números y recursivamente, también construye con list->chunks con los demás
;;números de la lista.
(define (list->chunks s n)
  (cond
    [(null? s) null]
    [(= 0 n) (error 'error"número no permitido")]
    [else
     (cons (take s n)
           (list->chunks (drop s n) n))]))

;;Problema 17
;;bundle : (listof (or unit-string? number?) integer? -> (listof (listof (or string? number?))
;;bundle recibe una lista s y un natural n, regresa una lista de sublistas de tamaño n.
;;Construye con la lista de los primeros n elementos y recursivamente, también construye con bundle con los demás elementos
;;de la lista
(define (bundle s n)
  (cond
    [(null? s) null]
    [(= n 0) (error 'bundle "número no admisible")]
    [else
     (if (unit-string-list? s)
         (cons (implode (take s n))
          (bundle (drop s n) n))
         (list->chunks s n))]))

;;Problema 7
;;partition : string integer? -> (listof (listof string?))
;;partition recibe una cadena y un natural n, partition regresa una lista de cadenas de caracteres de tamaño n.
;;Construye una lista con los primeros n caracteres y con recursividad usando partition usando los demás
;;caracteres de la cadena.
(define (partition s n)
  (unless (string? s)
    (error 'explode "esperaba una cadena, pero recibí: ~e" s))
  (cond
    [(= (string-length s) 0) null]
    [(< (string-length s) n) (list s)]
    [else (cons (substring s 0 n) (partition (substring s n) n))]))

;;Problema 8
;;isort : (listof number?) boolean? -> (listof number?)
;;isort recibe una lista y un booleano, regresa una lista ordenada de forma ascendente o descendiente
;;Ordena la lista con el primer número de la lista y el resultado de usar recursivamente isort con el resto de la lista.
(define (isort ls pred)
  (if (empty? ls)
      null
      (insert (first ls)
              (isort (rest ls) pred)
              pred)))

;;Problema 8
;;insert : number? (listof number?) boolean? -> (listof number?)
;;insert recibe un elemento, una lista y un booleano, regresa una lista con el elemento n insertado en la lista.
;;Checa si el elemento es menor o igual a el primer elemento de la lista, de ser así, añade el elemento antes o después del primer
;;elemento de la lista, caso contrario continúa con el siguiente elemento.
(define (insert n ls pred)
  (cond
    [(eq? pred #t)
     (cond
       [(empty? ls) (list n)]
       [(<= n (first ls)) (cons n ls)]
       [else (cons (first ls) (insert n (rest ls) pred))])]
    [(eq? pred #f)
     (cond
       [(empty? ls) (list n)]
       [(>= n (first ls)) (cons n ls)]
       [else (cons (first ls) (insert n (rest ls) pred))])]))

;;Problema10
;;smallers : (listof number?) number? -> (listof number?)
;;smallers recibe una lista y un número n y regresa una lista con los elementos menores al pivote.
;;Comprueba si los elementos de la lista son menores al pivote, en caso de ser así, construye una lista con
;;el primer elemento de la lista y recursivamente usa smallers con el resto de la lista y el mismo pivote,
;;en caso contrario pasa al siguiente elemento de la lista.
(define (smallers ls pivot)
  (cond
    [(null? ls) ls]
    [(< (first ls) pivot) (cons (first ls) (smallers (rest ls) pivot))]
    [else (smallers (rest ls) pivot)]))

;;Problema10
;;largers : (listof number?) number? -> (listof number?)
;;largers recibe una lista y un número pivot, regresa una lista con los elementos mayores al pivote.
;;Comprueba si los elementos de la lista son mayores al pivote, en caso de ser así, construye una lista
;;con el primer elemento de la lista y, recursivamente, usa largers con el resto de la lista,
;;en caso contrario pasa al siguiente elemento de la lista.
(define (largers ls pivot)
  (cond
    [(null? ls) ls]
    [(> (first ls) pivot) (cons (first ls) (largers (rest ls) pivot))]
    [else (largers (rest ls) pivot)]))

(define (quicksort ls)
  (cond
    [(empty? ls) null]
    [else
     (define pivot (first ls))
     (append (quicksort (smallers ls pivot))
             (list pivot)
             (quicksort (largers ls pivot)))]))

(define (equallers ls pivot)
  (cond
    [(null? ls) ls]
    [(= (first ls) pivot) (cons pivot (equallers (rest ls) pivot))]
    [else (equallers (rest ls) pivot)]))

;;Problema 11
;;quicksortfix : (listof number?) -> (listof number?)
;;quicksortfix recibe una lista de números y regresa una lista ordenada en orden ascendente.
;;Define al pivote como el primero de la lista, une 3 listas, una usando recursividad con quicksortfix pero con una lista
;;que solo tiene elementos menores al pivote, una lista que tiene a los que son iguales al pivote y otra usando recursividad
;;con quicksortfix pero con una lista que solo tiene elementos mayores al pivote.
(define (quicksortfix ls)
  (cond
    [(empty? ls) null]
    [else
     (define pivot (first ls))
     (append (quicksortfix (smallers ls pivot))
             (equallers ls pivot)
             (quicksortfix (largers ls pivot)))]))

(define (smallers2 ls pivot)
  (cond
    [(null? ls) ls]
    [(string<? (substring (first ls) 0 1) (substring pivot 0 1)) (cons (first ls) (smallers2 (rest ls) pivot))]
    [else (smallers2 (rest ls) pivot)]))

(define (largers2 ls pivot)
  (cond
    [(null? ls) ls]
    [(string>? (substring (first ls) 0 1) (substring pivot 0 1)) (cons (first ls) (largers2 (rest ls) pivot))]
    [else (largers2 (rest ls) pivot)]))

(define (equallers2 ls pivot)
  (cond
    [(null? ls) ls]
    [(string=? (substring (first ls) 0 1) (substring pivot 0 1)) (cons pivot (equallers2 (rest ls) pivot))]
    [else (equallers2 (rest ls) pivot)]))

;;Problema 12
;;quicksort2 : (listof number?) boolean? -> (listof number?s)
;;quicksort2 recibe una lista y un booleano, regresa una lista ordenada en orden ascendente o descendente.
;;Define al pivote como el primero de la lista, une 3 listas, una usando recursividad con quicksortfix pero con una lista
;;que solo tiene elementos menores al pivote, una lista que tiene a los que son iguales al pivote y otra usando recursividad
;;con quicksortfix pero con una lista que solo tiene elementos mayores al pivote, el orden de las listas puede invertirse
;;dependiendo del booleano.
(define (quicksort2 ls p)
  (cond
    [(empty? ls) ls]
    [else
     (define pivot (first ls))
     (cond
       [(empty? ls) null]
       [(eq? string>? p)
        (append (quicksort2 (smallers2 ls pivot) p)
                (equallers2 ls pivot)
                (quicksort2 (largers2 ls pivot) p))]
       [(eq? string<? p)
        (append (quicksort2 (largers2 ls pivot) p)
                (equallers2 ls pivot)
                (quicksort2 (smallers2 ls pivot) p))]
       [(eq? > p)
        (append (quicksort2 (smallers ls pivot) p)
                (equallers ls pivot)
                (quicksort2 (largers ls pivot) p))]
       [(eq? < p)
        (append (quicksort2 (largers ls pivot) p)
                (equallers ls pivot)
                (quicksort2 (smallers ls pivot) p))])]))
;13

;función para crear una lista de numeros aleatorios para saber el umbral
(define (randomlist n mx)
  (for/list ((i n))
    (add1 (random mx))))

;;Problema 13
;;quicksort3 : (listof number?) -> (listof number?)
;;quicksort3 recibe una lista y regresa una lista ordenada de forma ascendente.
;;Si el número de elementos de la lista es menor o igual a 100, usa el procedimiento isort, en caso
;;contrario usa quicksortfix.
(define (quicksort3 ls)
  (cond
    [(empty? ls) ls]
    [(>= 100 (length ls)) (isort ls #t)]
    [else (quicksortfix ls)]))

;;Problema 14
;;smallers3 : (listof number?) boolean? -> (listof number?)
;;smallers3 recibe una lista y un número n, devuelve una lista de números menores a n.
;;Usa filter y usa como predicado a lambda, el cual checa si x, el cual es el elemento actual de la lista, si es menor al
;;pivot, esto actúa sobre toda la lista.
(define (smallers3 ls pivot)
  (filter (lambda (x) (< x pivot)) ls))

;;Problema 14
;;largers3 : (listof number?) boolean? -> (listof number?)
;;largers3 recibe una lista y un número n, devuelve una lista de números mayores a n.
;;Usa filter y usa como predicado a lambda, el cual checa si x, el cual es el elemento actual de la lista, si es mayor al
;;pivot, esto actúa sobre toda la lista.
(define (largers3 ls pivot)
  (filter (lambda (x) (> x pivot)) ls))

;;Problema 15
;;quicksort4 : (listof number?) -> (listof number?)
;;quicksort4 recibe una lista y devuelve una lista ordenada de forma ascendente.
;;Define a pivot como el primero de la lista, para después unir 3 listas, una la cual usa recursividad con quicksort4 que tiene como procedimiento
;;utilizado smallers3, una lista la cual contiene a todos los elementos que son iguales al pivote y otra lista que usa recursividad con quicksort4 que tiene
;;como procedimiento utilizado largers3.
(define (quicksort4 ls)
  (cond
    [(empty? ls) null]
    [else
     (define pivot (first ls))
     (append (quicksort4 (filter (lambda (x) (< x pivot)) ls))
             (equallers ls pivot)
             (quicksort4 (filter (lambda (x) (> x pivot)) ls)))]))


(define (smallerss l n)
  (cond
    [(empty? l) '()]
    [else (if (<= (first l) n)
              (cons (first l) (smallers (rest l) n))
              (smallers (rest l) n))]))

(provide (all-defined-out))