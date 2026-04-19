#lang typed/racket
(require typed/rackunit)

;; Parse000
; Develop the parse000 function
; accept s expression, return boolean symbol
; Sexp for s expression
(define (parse000 [s : Sexp]) : Boolean
    (match s
        ; list containing number, symbol ('chris), symbol
        [(list (? real?) 'chris (? symbol?)) #t]
        [_ #f]))

;; Test cases
(check-equal? (parse000 '(1 chris hello)) #t)
(check-equal? (parse000 '(1 chris hello)) #t)