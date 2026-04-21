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
(check-equal? (parse000 '(10 chris world)) #t)
(check-equal? (parse000 '(1 james hello)) #f)
(check-equal? (parse000 '(1 chris "hello")) #f)



;; Parse001
; Develop the parse001 function
; accept s expression, return the symbol if success, #f is not
; use union type
(define (parse001 [s : Sexp]) : (U Symbol False)
    (match s
        [(list (? real?) 'chris (? symbol? x)) x]
        [_ #f]))

;; Test cases
(check-equal? (parse001 '(1 chris hello)) 'hello)
(check-equal? (parse001 '(10 chris world)) 'world)
(check-equal? (parse001 '(1 james hello)) #f)
(check-equal? (parse001 '(1 chris "hello")) #f)



;; Parse 002
; develop the parse002 function
; uses single pattern
; s expression are lists of three whose second element is a list of real numbers
; return list of number or return false if fail

; (define (my-fun [s : Sexp]) : (Listof Symbol)
;   (match s
;     [(list 'x1 (? symbol? rst) ...)
;      ;; cast must succeed by definition of pattern above
;      (cast rst (Listof Symbol))]
;     [(list (? symbol? fst) (? symbol? rst) ...)
;      (list fst)]))

(define (parse002 [s : Sexp]) : (U (Listof Real) False)
    (match s 
        [(list _ (list (? real? rst) ...) _)    ; 3 elements
            (cast rst (Listof Real))]
        [_ #f]))

;; Test Case
(check-equal? (parse002 '(a (1 2 3) b)) '(1 2 3))
(check-equal? (parse002 '(apple () bannana)) '())
(check-equal? (parse002 '(a (1 "2" 3) b)) #f)
(check-equal? (parse002 '(a (1 2 3) b c)) #f)


;; Develop the parse003 function
; list of lists exactly three numbers each
; return the sum of the first numbers in list, minus the sum of third on list
(define (parse003 [s : Sexp]) : (U Real False)
    (match s
        [(list (list (? real? first) _ (? real? third)) ...)
            (- (apply + (cast first (Listof Real)))
                (apply + (cast third (Listof Real)))
            )]
        [_ #f]))

;; Test Case
(check-equal? (parse003 '((1 2 3) (4 5 6))) -4)     ; (1 + 4) - (3 + 6)
(check-equal? (parse003 '((1 0 0) (0 0 1))) 0)
(check-equal? (parse003 '((1 2 3))) -2)
(check-equal? (parse003 '()) 0)
(check-equal? (parse003 '((1 2 3) ())) #f)
(check-equal? (parse003 '(1 2 3)) #f)



;; Develop the ohno function
; accept a value
; return the symbol 'okay if input is number
; uses error to signal an error include given value
(define (ohno [value : Any]) : Symbol
    (if (real? value)
        'okay
        (error 'ohno "expected number, got ~e" value)))

(check-exn (regexp (regexp-quote "ohno: expected number, got \"Ouch my foot\"")) 
            (lambda () (ohno "Ouch my foot")))

(check-exn (regexp (regexp-quote "ohno: expected number, got '(1 2 3)")) 
            (lambda () (ohno '(1 2 3))))

(check-equal? (ohno 7) 'okay)



;; Define the Arith language described in the textbook in chapter 3
; (define-type ArithC
;   [numC (n : number)]
;   [plusC (l : ArithC) (r : ArithC)]
;   [multC (l : ArithC) (r : ArithC)])

(define-type ArithC (U numC plusC multC sqrC))
(struct numC ([n : Real]) #:transparent)
(struct plusC ([l : ArithC] [r : ArithC]) #:transparent)
(struct multC ([l : ArithC] [r : ArithC]) #:transparent)
(struct sqrC ([arg : ArithC]) #:transparent)

;; Develop the evaluation method described in the textbook
; call the function interp
; (define (interp [a : ArithC]) : number
;   (type-case ArithC a
;     [numC (n) n]
;     [plusC (l r) (+ (interp l) (interp r))]
;     [multC (l r) (* (interp l) (interp r))]))

(define (interp [a : ArithC]) : Real
    (match a
        [(numC n) n]
        [(plusC l r) (+ (interp l) (interp r))]
        [(multC l r) (* (interp l) (interp r))]
        [(sqrC a) (let ([val (interp a)]) (* val val))]))

(check-equal? (interp (numC 1)) 1)
(check-equal? (interp (plusC (numC 1) (numC 2))) 3)
(check-equal? (interp (multC (numC 2) (numC 3))) 6)


;; Develop the method swap-adds
; accept an ArithC and return a new ArithC
; left and right term of every addition are swapped
(define (swap-add [a : ArithC]) : ArithC
    (match a
        [(numC n) (numC n)]
        [(plusC l r) (plusC (swap-add r) (swap-add l))]
        [(multC l r) (multC (swap-add l) (swap-add r))]))

(check-equal? (swap-add (numC 1)) (numC 1))
(check-equal? (swap-add (plusC (numC 1) (numC 3))) (plusC (numC 3) (numC 1)))
(check-equal? (swap-add (multC (numC 2) (numC 3))) (multC (numC 2) (numC 3)))

;; Develop the method swap-adds
; Add a printf to your swap-adds
(define (swap-add-printf [a : ArithC]) : ArithC
    (begin
        (printf "entry enter: ~e\n" a)
        (match a
            [(numC n) (numC n)]
            [(plusC l r) (plusC (swap-add r) (swap-add l))]
            [(multC l r) (multC (swap-add l) (swap-add r))])))

(check-equal? (swap-add-printf (numC 1)) (numC 1))
(check-equal? (swap-add-printf (plusC (numC 1) (numC 3))) (plusC (numC 3) (numC 1)))
(check-equal? (swap-add-printf (multC (numC 2) (numC 3))) (multC (numC 2) (numC 3)))



;; Develop a parser for the Arith language
(define (parser [s : Sexp]) : ArithC
    (match s
        [(? real? n) (numC n)]
        [(list '+ l r) (plusC (parser l) (parser r))]
        [(list '* l r) (multC (parser l) (parser r))]
        [(list '^2 a) (sqrC (parser a))]
        [else (error 'parser "invalid input: ~e" s)]))


;; Develop the one-line function top-interp
(define (top-interp [s : Sexp]) : Real
    (interp (parser s)))

(check-equal? (top-interp '1) 1)
(check-equal? (top-interp '{+ 1 2}) 3)
(check-equal? (top-interp '{* 2 3}) 6)
(check-equal? (top-interp '{^2 3}) 9)
(check-exn (regexp (regexp-quote "parser: invalid input: '(+ 1 2 3)"))
            (lambda () (top-interp '{+ 1 2 3})))

;; Develop the zip function
; two list of Number of same length
; return a new list of list where each element of new list is a list containing element from orgigianl list (???)
; return a new list where each Number element in the list matches the original list like a zip pairing.
; list1 = (1 2 3)
; list2 = (4 5 6)
; res = ((1 4) (2 5) (3 6))

(define (zip [list1 : (Listof Number)] [list2 : (Listof Number)]) : (Listof (List Number Number))
    (match (list list1 list2)
        [(list '() '()) '()]    ; check if list are empty
        [(list (cons first1 rest1) (cons first2 rest2))
            (cons (list first1 first2) (zip rest1 rest2))]
        [_ (error 'zip "lists not same length")]))

(check-equal? (zip '(1 2 3) '(4 5 6)) '((1 4) (2 5) (3 6)))
(check-equal? (zip '() '()) '())
(check-exn (regexp (regexp-quote "zip: lists not same length"))
            (lambda () (zip '(1) '(1 2))))
