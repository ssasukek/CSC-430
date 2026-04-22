;; TODO
; error messages must contain the string "VEBG"
; no mutation
; write a parser and interpreter
; main -> consist of list of functions, no two have same name
; conditonals -> suppost ifleq0? construct
; use lab3 code (s-expression and returns ExprC)
; binop (binary operator) 


#lang typed/racket
(require typed/rackunit)

;; Definition of abstract syntax (AST) data structure
(define-type ExprC (U numC idC ifleq0 binopC appC))
(struct numC ([n : Real]) #:transparent)
(struct idC ([sym : Symbol]) #:transparent)
(struct ifleq0 ([test : ExprC] [then : ExprC] [else : ExprC]) #:transparent)
(struct binopC ([opr : ExprC] [l : ExprC] [r : ExprC]) #:transparent)
(struct appC ([fname : Symbol] [arg : ExprC]) #:transparent)

;; Add a fundefC structure
(struct FunDefC ([name : Symbol] [param : Symbol] [body : ExprC]) #:transparent)


;; lookup table for operator
(define (lookup-opr [opr : Symbol]) : Real
    (match opr
        []))

;; Parser - handle zero or more arg
; parses an expression (not done)
(define (parse [s : Sexp]) : ExprC
    (match s
        [(? real? n) (numC n)]
        [(? symbol? s) (idC s)]
        [(list 'ifleq0? test then else) (ifleq0 (parse test) (parse then) (parse else))]
        [(list 'binopC) ()]
        [(list 'appC) ()]
        [other (error 'parse "Syntax error, given invalid term ~e" other)]))
        
; parses a function definition (not done)
(define (parse-fundef [s : Sexp]) : FunDefC
  (match s
    [(list 'named-fn (? symbol? fname) (? symbol? param) body)
        (FunDefC fname param (parse body))]
    [other (error 'parse-fundef "BOOL failed to parse fundef ~e" other)]))


; parse whole program (not done)
(define (parse-prog [s : Sexp]) : (Listof FunDefC)
  (match s
    []
    [other (error 'parse-prog "VEBG: ...")]
  ))


;; Subsitution (ch5) (not done)
(define (subst [what : ExprC] [for : symbol] [in : ExprC]) : ExprC
  (match in
    [(numC n) in]
    [(idC s) (cond
                [(symbol=? s for) what]
                [else in])]
    [(binopC opr l r)]
    [(ifleq0 test then else)]
    [(appC fname arg) (appC fname (subst what for args))]))


;; Interpret (not fix)
(define (interp [exp : ExprC] [fds : (Listof FunDefC)]) : Real
  (match exp
    [(numC n) n]
    [(idC s) (error 'interp "VEBG: BOOL unbound name ~e" s)]
    [(binopC opr l r) ((lookup-opr opr) (interp l fds) (interp r fds))]
    [(ifleq0 test then else)]
    [(appC fname arg)
        (define interpedArg (BoolC (interp arg fds)))
        (define fd (get-fundef fname fds))
        (define substFunBody (subst interpedArg
                                            (FunDefC-param fd)
                                            (FunDefC-body fd)))
        (interp substFunBody fds)]

  ))



; interp main from the fundefs
(define (interp-fns [fds : (Listof FunDefC)]) : Real
    (interp (appC 'main '()) fds))

;; entry point 
(define (top-interp [s : Sexp]) : Real
  (interp-fns (parse-prog s)))


;; Test Cases (check-equal?, check-=, or check-exn forms)
(check-equal? (interp-fns
               (parse-prog '{{named-fn f (x y) -> {+ x y}}
                             {named-fn main () -> {f 1 2}}}))
              3)
(check-equal? (interp-fns
               (parse-prog '{{named-fn f () -> 5}
                             {named-fn main () -> {+ {f} {f}}}}))
              10)


(check-exn #px"wrong arity"
           (λ ()
             (interp-fns
              (parse-prog '{{named-fn f (x y) -> {+ x y}}
                            {named-fn main () -> {f 1}}}))))