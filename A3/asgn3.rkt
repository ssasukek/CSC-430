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
(struct appC ([fname : Symbol] [arg : ExprC]))

;; Add a fundefC structure
(struct FunDefC ([name : Symbol] [param : Symbol] [body : ExprC]) #:transparent)

;; Parser - handle zero or more arg
; parses an expression
(define (parse [s : Sexp]) : ExprC
    (match s
        [(? real? n) (numC n)]
        [(? symbol? s) (idC s)]
        [(list 'ifleq0? test then else) (ifleq0 (parse test) (parse then) (parse else))]
        [(list 'binopC) ()]
        [(list 'appC) ()]
        [other (error 'parse "Syntax error, given invalid term ~e" other)]))
        
; parses a function definition
(define (parse-fundef [s : Sexp]) : FunDefC
  (match s
  ))

; parse whole program
(define (parse-prog [s : Sexp]) : (Listof FunDefC)
  (match s
  ))


;; Subsitution (ch5)
(define (subst [what : ExprC] [for : symbol] [in : ExprC]) : ExprC
  (match in
  [(numC n) in]
  [(idC s)]
  [(binopC opr l r)]
  [(ifleq0 test then else)]
  [(appC f arg)]
  ))



;; Interpret
(define (interp [exp : ExprC] [fds : (Listof FunDefC)]) : Real
  (match exp
  [(numC n) n]

  ))

; interp main from the fundefs
(define (interp-fns [fds : (Listof FunDefC)]) : Real
    (interp (appC 'main '()) fds))


(define (top-interp [s : Sexp]) : Real
  (interp-fns (parse-prog s)))




;; Test Cases (check-equal?, check-=, or check-exn forms)
;