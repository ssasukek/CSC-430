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
(struct binopC ([opr : ExprC] [l : ExprC] [r : ExprC]) #:transparent)
(struct appC ([fname : Symbol] [args : (Listof ExprC)]) #:transparent)

;; Add a fundefC structure
(struct FunDefC ([name : Symbol] [param : Symbol] [body : ExprC]) #:transparent)

;; Parser - handle zero or more args
; parses an expression (need test case)
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
        
; parses a function definition (not done?)
 (parse-fundef '{named-fn f (x y) -> x})
(define (parse-fundef [s : Sexp]) : FunDefC
  (match s
    [(list 'named-fn (? symbol? fname) (list (? symbol? param) ...) '-> body)
        (FunDefC fname param (parse body))]
    [other (error 'parse-fundef "VEBG: failed to parse fundef ~e" other)]))

; parse whole program
(define (parse-prog [s : Sexp]) : (Listof FunDefC)
  (match s
    ['() '()]
    [(cons fst rst) (cons (parse-fundef fst) (parse-prog rst))]
    [other (error 'parse-prog "VEBG: failed to parse program ~e" other)]
  ))


;; Subsitution (ch5) (not done)
(define (subst [what : ExprC] [for : symbol] [in : ExprC]) : ExprC
  (match in
    [(numC n) in]
    [(idC s) (cond
                [(symbol=? s for) what]
                [else in])]
    [(binopC opr l r) (binopC (subst l opr r))]
    [(ifleq0 test then else) (ifleq0 (subst then test else))]
    [(appC fname args) (appC fname (subst what for args))]))


;; Interpret (not done)
(define (interp [exp : ExprC] [fds : (Listof FunDefC)]) : Real
  (match exp
    [(numC n) n]
    [(idC s) (error 'interp "VEBG: unbound name ~e" s)]
    [(binopC opr l r) ((lookup-opr opr) (interp l fds) (interp r fds))]
    [(ifleq0 test then else)]
    [(appC fname args)
                    (define fd (get-fundef fname fds))
                    
                    ]
  ))


;; Helper Functions ;;
;; no two have the same name...
(define (duplication? [lst : (Listof Symbol)]) : Boolean
  (cond
    [(empty? lst) #f]
    [(member (first lst) (rest lst)) #t]
    [else (duplication? (rest lst))]))

;; lookup table for operator
(define (lookup-opr [opr : Symbol]) : Real
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






