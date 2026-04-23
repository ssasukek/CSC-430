;; TODO
; error messages must contain the string "VEBG"
; no mutation
; write a parser and interpreter
; main -> consist of list of functions, no two have same name
; conditonals -> suppost ifleq0? construct
; use lab3 code (s-expression and returns ExprC)
; binop (binary operator) 
; support multi arg and param thru subst
; eager


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

;; Parser - handle zero or more arg
; parses an expression (not done)
(define (parse [s : Sexp]) : ExprC
    (match s
        [(? real? n) (numC n)]
        [(? symbol? s) (idC s)]
        [(list 'ifleq0? test then else) (ifleq0 (parse test) (parse then) (parse else))]
        [(list 'binopC) ()]
        [(list 'appC) ()]
        [other (error 'parse "VEBG: Syntax error, given invalid term ~e" other)]))
        
; parses a function definition (not done?)
; (parse-fundef '{named-fn f (x y) -> x})
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
    [(binopC opr l r) ()]
    [(ifleq0 test then else) ()]
    [(appC fname arg) (appC fname (subst what for args))]))


;; Interpret (not done)
(define (interp [exp : ExprC] [fds : (Listof FunDefC)]) : Real
  (match exp
    [(numC n) n]
    [(idC s) (error 'interp "VEBG: unbound name ~e" s)]
    [(binopC opr l r) ((lookup-opr opr) (interp l fds) (interp r fds))]
    [(ifleq0 test then else)]
    [(appC fname arg)
                    (define fd (get-fundef fname fds))
                    
                    ]
  ))

; interp main from the fundefs
(define (interp-fns [fds : (Listof FunDefC)]) : Real
    (interp (appC 'main '()) fds))

;; entry point 
(define (top-interp [s : Sexp]) : Real
  (interp-fns (parse-prog s)))


;; Helper Functions

;; handle multiple param / arg
(define (multi-subst [what : ExprC] [for : symbol] [in : ExprC]) : ExprC
    (cond
        [(empty? what) in]
        [else (multi-subst (rest what) (rest for) subst (first what) (first for) in)]))

;; lookup table for operator
; (define (lookup-opr [opr : Symbol]) : Real
;     (match opr
;         []))


;; find the function defintion in the list (from lecture)
(define (get-fundef [n : Symbol] [fds : (Listof FunDefC)]) : FunDefC
  (cond
    [(empty? fds)
     (error 'get-fundef "VEGB reference to undefined function ~e" n)]
       [(equal? n (FunDefC-name (first fds))) (first fds)]
       [else (get-fundef n (rest fds))]))






;; Test Cases (check-equal?, check-=, or check-exn forms)

; interp test
(check-equal? (interp-fns
               (parse-prog '{{named-fn f (x y) -> {+ x y}}
                             {named-fn main () -> {f 1 2}}}))
              3)
(check-equal? (interp-fns
               (parse-prog '{{named-fn f () -> 5}
                             {named-fn main () -> {+ {f} {f}}}}))
              10)



; parse test
(check-equal? (parse '1) (numC 5))
(check-equal? (parse 'x) (numC x))
(check-equal? (parse '{+ 1 2}) (binopC '+ (numC 1) (numC 2)))


; subst tests
(check-equal? (subst (numC 1) 'x (idC 'x)) (numC 1))


; fundef test



(check-exn #px"wrong arity"
           (λ ()
             (interp-fns
              (parse-prog '{{named-fn f (x y) -> {+ x y}}
                            {named-fn main () -> {f 1}}}))))
