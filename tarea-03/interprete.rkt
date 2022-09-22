#lang plait

;; TIPO VALUE ;;
(define-type Value
  (numV [value : Number])
  (strV [value : String])
  (boolV [value : Boolean])
  (funV [param : Symbol]
        [body : ExprC]
        [env : Environment]))


;; TIPO OPERADOR ;;
(define-type Operator
  (plusO)
  (appendO)
  (numeqO)
  (streqO))

;; LENGUAJE SUPERFICIAL ;;
(define-type ExprS
  (numS [value : Number])
  (boolS [value : Boolean])
  (strS [value : String])
  (idS [name : Symbol])
  (ifS [a : ExprS] [b : ExprS] [c : ExprS])
  (andS [right : ExprS] [left : ExprS])
  (orS [left : ExprS] [right : ExprS])
  (binopS [op : Operator] [left : ExprS] [right : ExprS])
  (funS [param : Symbol] [body : ExprS])
  (letS [name : Symbol] [value : ExprS] [body : ExprS])
  (appS [func : ExprS] [arg : ExprS]))

;; LENGUAJE NUCLEO ;;
(define-type ExprC
  (numC [value : Number])
  (boolC [value : Boolean])
  (strC [value : String])
  (idC [name : Symbol])
  (ifC [a : ExprC] [b : ExprC] [c : ExprC] [type : ExprC])
  (binopC [op : Operator] [left : ExprC] [right : ExprC])
  (funC [param : Symbol] [body : ExprC])
  (appC [func : ExprC] [arg : ExprC]))

;; EVALUADOR ;;

(define (eval [str : S-Exp]) : Value
  (interp (desugar (parse str))))

;; Entorno ;;
(define-type Binding
  (binding [name : Symbol]
           [value : Value]))

(define-type-alias Environment (Listof Binding))

(define empty-env empty)

(define (lookup-env name env)
  (if (empty? env)
      (unbound-identifier-error name)
      (if (eq? name (binding-name (first env)))
          (binding-value (first env))
          (lookup-env name (rest env)))))

(define (extend-env name value env)
  (cons (binding name value) env))

(define (bad-conditional-error [v : Value])
  (error 'interp
         (string-append
          "bad conditional to if expression: "
          (to-string v))))

(define (unbound-identifier-error [name : Symbol])
  (error 'interp
         (string-append
          "identificador no está enlazado"
          (to-string name))))

;; INTERP ;;
(define (interp [str : ExprC]) : Value
  (interp-helper str empty-env)
  )

(define (interp-helper [str : ExprC] [env : Environment]) : Value
  (type-case ExprC str
    [(numC value) (numV value)]
    [(strC value) (strV value)]
    [(boolC value)(boolV value)]
    [(idC name) (lookup-env name env)]
    [(ifC a b c type)
     (let ([a (interp-helper a env)])
       (let ([type (interp-helper type env)])
         (interp-if a b c type env)))]
    [(binopC op left right)
     (let ([left (interp-helper left env)])
       (let ([right (interp-helper right env)])
         (interp-binop op left right)))]
    [(funC param body) (funV param body env)]
    [(appC func arg)
     (let ([v1 (interp-helper func env)])
       (if (funV? v1)
           (interp-helper (funV-body v1) (extend-env (funV-param v1) (interp-helper arg env) (funV-env v1)))
           (error 'interp "no es una función")))]))

(define (interp-if [a : Value] [b : ExprC] [c : ExprC] [type : Value] [env : Environment]) : Value
  (if (not (boolV? a))
      (error 'interp "no es un valor booleano")
      (cond
        [(= (numV-value type) 0) (if (boolV-value a)
                                     (interp-helper b env)
                                     (interp-helper c env))]
        [(= (numV-value type) 1) (if (boolV-value a)
                                     (if (boolV? (interp-helper b env))
                                         (if (boolV-value (interp-helper b env))
                                             a
                                             (interp-helper c env))
                                         (error 'interp "no es un valor booleano"))
                                     (interp-helper c env))]
        [(= (numV-value type) 2) (if (boolV-value a)
                                     (interp-helper b env)
                                     (if (boolV? (interp-helper c env))
                                         (if (boolV-value (interp-helper c env))
                                             (interp-helper b env)
                                             a)
                                         (error 'interp "no es un valor booleano")))])))

(define (interp-binop [op : Operator] [left : Value] [right : Value]) : Value
  (type-case Operator op
    [(plusO)
     (if (numV? left)
         (if (numV? right)
             (numV (+ (numV-value left) (numV-value right)))
             (error 'binop "argumento incorrecto"))
         (error '_ "argumento incorrecto"))]
    [(appendO)
     (if (strV? left)
         (if (strV? right)
             (strV (string-append (strV-value left) (strV-value right)))
             (error 'binop "argumento incorrecto"))
         (error 'binop "argumento incorrecto"))]
    [(numeqO)
     (if (numV? left)
         (if (numV? right)
             (boolV (= (numV-value left) (numV-value right)))
             (error 'binop "argumento incorrecto"))
         (error 'binop "argumento incorrecto"))]
    [(streqO)
     (if (strV? left)
         (if (strV? right)
             (boolV (string=? (strV-value left) (strV-value right)))
                    (error 'binop "argumento incorrecto"))
         (error 'binop "argumento incorrecto"))]))

;; DESUGAR ;;
(define (desugar [str : ExprS]) : ExprC
  (type-case ExprS str
    [(numS value) (numC value)]
    [(boolS value) (boolC value)]
    [(strS value) (strC value)]
    [(idS name) (idC name)]
    [(ifS a b c) (ifC (desugar a) (desugar b) (desugar c) (numC 0))]
    [(andS a b) (ifC (desugar a) (desugar b) (boolC #f) (numC 1))]
    [(orS a b) (ifC (desugar a) (boolC #t) (desugar b) (numC 2))]
    [(binopS op left right) (binopC op (desugar left) (desugar right))]
    [(funS param body) (funC param (desugar body))]
    [(letS name value body)(appC (funC name (desugar body)) (desugar value))]
    [(appS func arg) (appC (desugar func) (desugar arg))]))

;; PARSE ;;
(define (parse [in : S-Exp]) : ExprS
  (cond
    [(s-exp-number? in) (parse-number in)]
    [(s-exp-string? in) (parse-string in)]
    [(s-exp-match? `true in) (boolS #t)]
    [(s-exp-match? `false in) (boolS #f)]
    [(s-exp-match? `{if ANY ...} in) (parse-if in)]
    [(s-exp-match? `{and ANY ...} in) (parse-and in)]
    [(s-exp-match? `{or ANY ...} in) (parse-or in)]
    [(s-exp-match? `{+ ANY ...} in) (parse-+ in)]
    [(s-exp-match? `{++ ANY ...} in) (parse-++ in)]
    [(s-exp-match? `{num= ANY ...} in) (parse-num= in)]
    [(s-exp-match? `{str= ANY ...} in) (parse-str= in)]
    [(s-exp-match? `{fun ANY ...} in) (parse-fun in)]
    [(s-exp-match? `{let {SYMBOL ANY} ANY ...} in) (parse-let in)]
    [(s-exp-match? `{ANY ...} in) (parse-app in)]
    [(s-exp-symbol? in) (parse-id in)]
    [else (error 'parse "expresión malformada")]))

(define (parse-number in)
  (numS (s-exp->number in)))

(define (parse-string in)
  (strS (s-exp->string in)))

(define (parse-id in)
  (idS (s-exp->symbol in)))

(define (parse-if in)
  (let ([inlst (s-exp->list in)])
    (if (equal? (length inlst) 4)
        (ifS (parse (second inlst))
             (parse (third inlst))
             (parse (fourth inlst)))
        (error 'parse "cantidad incorrecta de argumentos para if"))))

(define (parse-and in)
  (let ([inlst (s-exp->list in)])
    (if (equal? (length inlst) 3)
        (andS (parse (second inlst)) (parse (third inlst)))
        (error 'parse "cantidad incorrecta de argumentos para and"))))

(define (parse-or in)
  (let ([inlst (s-exp->list in)])
    (if (equal? (length inlst) 3)
        (orS (parse (second inlst)) (parse (third inlst)))
        (error 'parse "cantidad incorrecta de argumentos para or"))))

(define (parse-+ in)
  (let ([inlst (s-exp->list in)])
    (if (equal? (length inlst) 3)
        (binopS (plusO) (parse (second inlst)) (parse (third inlst)))
        (error 'parse "cantidad incorrecta de argumentos para +"))))

(define (parse-++ in)
  (let ([inlst (s-exp->list in)])
    (if (equal? (length inlst) 3)
        (binopS (appendO) (parse (second inlst)) (parse (third inlst)))
        (error 'parse "cantidad incorrecta de argumentos para +"))))

(define (parse-num= in)
  (let ([inlst (s-exp->list in)])
    (if (equal? (length inlst) 3)
        (binopS (numeqO) (parse (second inlst)) (parse (third inlst)))
        (error 'parse "cantidad incorrecta de argumentos para num="))))

(define (parse-str= in)
  (let ([inlst (s-exp->list in)])
    (if (equal? (length inlst) 3)
        (binopS (streqO) (parse (second inlst)) (parse (third inlst)))
        (error 'parse "cantidad incorrecta de argumentos para str="))))

(define (parse-fun in)
  (cond
    [(s-exp-match? `{fun SYMBOL ANY ...} in)
     (let ([inlst (s-exp->list in)])
       (if (equal? (length inlst) 3)
           (funS (s-exp->symbol (second inlst)) (parse (third inlst)))
           (error 'parse "funciones deben tener solo un cuerpo")))]
    [(s-exp-match? `{fun ANY ...} in)
     (error 'parse "parametros a función deben ser símbolos")]))

(define (parse-let in)
  (let ([inlst (s-exp->list in)])
    (if (equal? (length inlst) 3)
        (letS
         (s-exp->symbol (first (s-exp->list (second inlst))))
         (parse (second (s-exp->list (second inlst))))
         (parse (third inlst)))
        (error 'parse "cantidad incorrecta de argumentos para let"))))

(define (parse-app in)
  (let ([inlst (s-exp->list in)])
    (if (equal? (length inlst) 2)
        (appS (parse (first inlst)) (parse (second inlst)))
        (error 'parse "cantidad incorrecta de argumentos en aplicación de funciones"))))