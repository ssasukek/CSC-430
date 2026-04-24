;; TODO
; error messages must contain the string "VEBG"
; no mutation
; write a parser and interpreter
; main -> consist of list of functions, no two have same name
; conditonals -> suppost ifleq0? construct
; use lab3 code (s-expression and returns ExprC)
; binop (binary operator) 
; support multi args and param thru subst
; eager


#lang typed/racket
(require typed/rackunit)

;; Definition of abstract syntax (AST) data structure
(define-type ExprC (U numC idC ifleq0 binopC appC))
(struct numC ([n : Real]) #:transparent)
(struct idC ([sym : Symbol]) #:transparent)
(struct ifleq0 ([test : ExprC] [then : ExprC] [else : ExprC]) #:transparent)
(struct binopC ([opr : Symbol] [l : ExprC] [r : ExprC]) #:transparent)
(struct appC ([fname : Symbol] [args : (Listof ExprC)]) #:transparent)

;; Add a fundefC structure
(struct FunDefC ([name : Symbol] [param : (Listof Symbol)] [body : ExprC]) #:transparent)

;; Parser - handle zero or more args
; parses an expression (need test)
(define (parse [s : Sexp]) : ExprC
    (match s
        [(? real? n) (numC n)]
        [(? symbol? s) (idC s)]
        [(list 'ifleq0? test then else) (ifleq0 (parse test) (parse then) (parse else))]
        [(list (? symbol? opr) l r) (binopC opr (parse l) (parse r))]
        [(list (? symbol? fname) args ...) 
          (appC fname
            (for/list : (Listof ExprC) ([sexp (cast args (Listof Sexp))])
              (parse sexp)))]
        [other (error 'parse "VEBG: Syntax error, given invalid term ~e" other)]))

;; Test cases for parse



; parses a function definition (almost done)
;(parse-fundef '{named-fn f (x y) -> x})
(define (parse-fundef [s : Sexp]) : FunDefC
  (match s
    [(list 'named-fn (? symbol? fname) (list (? symbol? param) ...) '-> body)
      (cond 
        [(duplication? (cast param (Listof Symbol)))
          (error 'parse-fundef "VEBG: duplicate parameter name in function ~e" fname)]
        [else
          (FunDefC fname (cast param (Listof Symbol)) (parse body))])]
    [other (error 'parse-fundef "VEBG: failed to parse fundef ~e" other)]))

;; Test cases for parse-fundef





; parse whole program
(define (parse-prog [s : Sexp]) : (Listof FunDefC)
  (match s
    ['() '()]
    [(cons fst rst) (cons (parse-fundef fst) (parse-prog rst))]
    [other (error 'parse-prog "VEBG: failed to parse program ~e" other)]
  ))

;; Test cases for parse-prog


;; Subsitution (ch5) (support multiple or zero argument) (almost done)
(define (subst [what : (Listof ExprC)] [for : (Listof Symbol)] [in : ExprC]) : ExprC
  (match in
    [(numC n) in]
    [(idC s)
      (cond
        [(empty? for) in]
        [(symbol=? s (first for)) (first what)]
        [else (subst (rest what) (rest for) in)])]

    [(binopC opr l r) 
      (binopC opr (subst what for l) (subst what for r))]

    [(ifleq0 test then else) 
      (ifleq0 (subst what for test) (subst what for then) (subst what for else))]

    [(appC fname args)
      (define (subst-args [args-lst : (Listof ExprC)]) : (Listof ExprC)
        (match args-lst
          ['() '()]
          [(cons fst rst) (cons (subst what for fst) (subst-args rst))]))
      (appC fname (subst-args args))]))

;; Test cases for subst + support multiple or zero argument




;; Interpret (not done)
(define (interp [exp : ExprC] [fds : (Listof FunDefC)]) : Real
  (match exp
    [(numC n) n]
    [(idC s) (error 'interp "VEBG: unbound name ~e" s)]
    [(binopC opr l r)
      (match opr
        ['+ (+ (interp l fds) (interp r fds))]
        ['- (- (interp l fds) (interp r fds))]
        ['* (* (interp l fds) (interp r fds))]
        ['/ (/ (interp l fds) (interp r fds))]
        [other (error 'interp "VEBG: invalid operator ~e" other)])]

    [(ifleq0 test then else)
      (if (<= (interp test fds) 0)
        (interp then fds)
        (interp else fds))]

    [(appC fname args)
      ; 1 evaluate the arguments (for eager)
      (define (interp-args [args-lst : (Listof ExprC)]) : (Listof ExprC)
        (match args-lst
          ['() '()]
          [(cons fst rst) (cons (numC (interp fst fds)) (interp-args rst))]))
      ; 2 lookup function body
      (define fd (get-fundef fname fds))
      (define params (FunDefC-param fd))
      ; 3 substitute the argument for the parameter
      (define subst-body (subst (interp-args args) params (FunDefC-body fd)))
      ; 4 interp the body
      (interp subst-body fds)
      (error 'interp "VEBG: function not implemented ~e" fname)]))

;; Test case for interp



;; Helper Functions ;;
;; no two have the same name...
(define (duplication? [lst : (Listof Symbol)]) : Boolean
  (cond
    [(empty? lst) #f]
    [(member (first lst) (rest lst)) #t]
    [else (duplication? (rest lst))]))

;; lookup table for operator
(define (lookup-opr [opr : Symbol]) : (Real -> Real)
    (match opr
        ['+ +]
        ['- -]
        ['* *]
        ['/ /]
        [other (error 'lookup-opr "VEBG: invalid operator ~e" other)]))

;; find the function defintion in the list (from lecture)
(define (get-fundef [n : Symbol] [fds : (Listof FunDefC)]) : FunDefC
  (cond
    [(empty? fds)
     (error 'get-fundef "VEGB reference to undefined function ~e" n)]
    [(equal? n (FunDefC-name (first fds))) (first fds)]
    [else (get-fundef n (rest fds))]))

;; Test cases (helper functions)
(check-equal? (duplication? '(a b c d)) #f)




;interp main from the fundefs
(define (interp-fns [fds : (Listof FunDefC)]) : Real
    (interp (appC 'main '()) fds))

;; entry point 
(define (top-interp [s : Sexp]) : Real
  (interp-fns (parse-prog s)))


