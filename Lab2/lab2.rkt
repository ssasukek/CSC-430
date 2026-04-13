#lang typed/racket
(require typed/rackunit)

;; List Template
; (define (list-template [lst : (listof Any)]) : Any
;     (match lst
;         ['() ...]
;         [(cons first rest) (... first ... (list-template rest) ...)]))

;; 1. Develop the function rev-str-app
(define (rev-str-app [l : (Listof String)]) : String
    (match l
        ['() ""]        ; <look for> <return>
        [(cons f r) (string-append (rev-str-app r) f)]))

;; Test Case
(check-equal? (rev-str-app '("")) "")
(check-equal? (rev-str-app '("ball" "juice" "frog")) "frogjuiceball")
(check-equal? (rev-str-app '("ball" "" "frog")) "frogball")


;; 2. Use :print-type
; 1. What type does rev-str-app have?   the type is (-> (Listof String) String)
; 2. Does this make sense?              Yes, it tell the rev-str-app is type Listof String
; 3. What about the type of ’+’?        the type is a bunch of (case -> type
; 4. Why is it so long?                 Because '+' operate many path of adding integer, reals, numbers, etc.


;; 3. Develop representation for bicycles
(struct Trek ([num-wheels : Integer]) #:transparent)
(struct Bianchi ([num-wheels : Integer]) #:transparent)
(struct Gunnar ([num-wheels : Integer]) #:transparent)

; define the union type for bicyle 
(define-type Bicycle (U Trek Bianchi Gunnar))

; testing
(define trek-bike (Trek 2))
(define bianchi-bike (Bianchi 2))
(define gunnar-bike (Gunnar 2))

;; 4. develop the function only-treks (no filter)
(define (only-treks [l : (Listof Bicycle)]) : (Listof Bicycle)
    (match l
        ['() '()]
        [(cons f r)
            (if (Trek? f)
                (cons f (only-treks r))
                (only-treks r))]))
; Test case
(check-equal? (only-treks '()) '())
(check-equal? (only-treks (list trek-bike bianchi-bike gunnar-bike)) (list trek-bike))
(check-equal? (only-treks (list trek-bike trek-bike gunnar-bike)) (list trek-bike trek-bike))
(check-equal? (only-treks (list bianchi-bike gunnar-bike)) '())


;; 5. develop the function only-bianchis
(define (only-bianchis [l : (Listof Bicycle)]) : (Listof Bicycle)
    (match l
        ['() '()]
        [(cons f r)
            (if (Bianchi? f)
                (cons f (only-bianchis r))
                (only-bianchis r))]))
; Test case
(check-equal? (only-bianchis '()) '())
(check-equal? (only-bianchis (list trek-bike bianchi-bike gunnar-bike)) (list bianchi-bike))
(check-equal? (only-bianchis (list bianchi-bike bianchi-bike gunnar-bike)) (list bianchi-bike bianchi-bike))
(check-equal? (only-bianchis (list trek-bike gunnar-bike)) '())


;; 6. abstract over the two to obtain the function onlyThese
(define (onlyThese [l : (Listof Bicycle)] [x : (Any -> Boolean)]) : (Listof Bicycle)
    (match l
        ['() '()]
        [(cons f r)
            (if (x f)
            (cons f (onlyThese r x))
            (onlyThese r x))]))
; Test case
(check-equal? (onlyThese (list trek-bike bianchi-bike gunnar-bike) Trek?) (list trek-bike))
(check-equal? (onlyThese (list trek-bike bianchi-bike bianchi-bike) Bianchi?) (list bianchi-bike bianchi-bike))
(check-equal? (onlyThese (list trek-bike bianchi-bike gunnar-bike) Gunnar?) (list gunnar-bike))



;; 7. develope the function my-append that consumes two lists and returns second one to the first result
(define (my-append [l : (Listof Any)] [l2 : (Listof Any)]) : (Listof Any)
    (match l
        ['() l2]
        [(cons f r)
            (cons f (my-append r l2))]))
; Test case
(check-equal? (my-append '() '()) '())
(check-equal? (my-append '(a b c) '(d e f)) '(a b c d e f))
(check-equal? (my-append '(a b) '(c d e f)) '(a b c d e f))
(check-equal? (my-append '(1 2) '()) '(1 2))


;; 8. develop the function my-take that consumes a list and a number n and return the first n element
(define (my-take [l : (Listof Any)] [n : Integer]) : (Listof Any)
    ;; (if <condition> <if true> <if false>)
    (if (<= n 0)
    '()
    (match l
        ['() '()]
        [(cons f r)
            (cons f (my-take r (- n 1)))])))
; Test case
(check-equal? (my-take '() 1) '())
(check-equal? (my-take '(a b c) 2) '(a b))
(check-equal? (my-take '(a b c) 0) '())


;; 3.1
; Why did we tell you not to use match in writing the only-treks function? 
    ; What happens if you don’t?

; Does your implementation of the my-take function call length? 
    ; If so, what is the asymptotic running time of your my-take function?

; How many of these functions can be implemented using map, filter or foldl ?

; How many of these functions can be implemented using for/list ?