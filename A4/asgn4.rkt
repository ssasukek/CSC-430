#lang typed/racket
(require typed/rackunit)

;; Definition of abstract syntax (AST) data structure
(define-type ExprC (U numC idC ifleq0 binopC appC))
(struct numC ([n : Real]) #:transparent)
(struct idC ([sym : Symbol]) #:transparent)
(struct ifleq0 ([test : ExprC] [then : ExprC] [else : ExprC]) #:transparent)
(struct binopC ([opr : ExprC] [l : ExprC] [r : ExprC]) #:transparent)
(struct appC ([fname : Symbol] [arg : (Listof ExprC)]) #:transparent)

;; Add a fundefC structure
(struct FunDefC ([name : Symbol] [param : Symbol] [body : ExprC]) #:transparent)




(define interp [exp : ExprC] [fds : (Listof FunDefC)] [env : Environment]) : Real
    (match exp
        [(appC fname args) 
            (match (lookup fname fds))])