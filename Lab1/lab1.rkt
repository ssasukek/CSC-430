#lang typed/racket
(require typed/rackunit)

;; Exercise 15
;; Purpose:
;;  Compute the logical ==> (sunny => friday)
;;  return #true if sunny is false or friday is true
(define (==> [sunny : Boolean] [friday : Boolean]) : Boolean
    (or (not sunny) friday))

;; Test Case
(check-equal? (==> #true #false) #false)
(check-equal? (==> #false #true) #true)
(check-equal? (==> #false #false) #true)


;; Exercise 19
;; Purpose:
;;  Consumes a string + number i and inserts "_" at the ith position. 
(define (string-insert [str : String] [i : Integer]) : String
    (string-append (substring str 0 i) "_" (substring str i)))

;; Test Case
(check-equal? (string-insert "test1" 1) "t_est1")
(check-equal? (string-insert "test2" 2) "te_st2")
(check-equal? (string-insert "test3" 3) "tes_t3")


;; Exercise 27
;; Purpose:
;;  Collect all definitions and change them so all magic numbers 
;;  are refactored into constant def.

;; DrRacket's definitions
(define ATTENDEE 120)
(define PRICE 5.0)
(define ATTENDEE-CHANGES 15)
(define PRICE-CHANGES 0.1)
(define FIXED-COST 180)
(define COST-OF-ATTENDEE 0.04)

; (define (attendees ticket-price)
;   (- 120 (* (- ticket-price 5.0) (/ 15 0.1))))
(define (attendees [ticket-price : Real]) : Real
    (- ATTENDEE (* (- ticket-price PRICE) (/ ATTENDEE-CHANGES PRICE-CHANGES))))

(define (revenue [ticket-price : Real]) : Real
  (* ticket-price (attendees ticket-price)))

(define (cost [ticket-price : Real]) : Real
  (+ FIXED-COST (* COST-OF-ATTENDEE (attendees ticket-price))))

(define (profit [ticket-price : Real]) : Real
  (- (revenue ticket-price) (cost ticket-price)))

;; Test Case
(check-equal? (profit 3) 1063.2)
(check-equal? (profit 4) 889.2)
(check-equal? (profit 5) 415.2)



;; Intervals
(define (interest [amount : Real]) : Real
    (cond
        [(<= amount 1000) (* amount 0.04)]
        [(<= amount 5000) (* amount 0.045)]
        [else (* amount 0.05)]))

(check-= (interest 500) 20.0 0.001)
(check-= (interest 1000) 40.0 0.001)
(check-= (interest 15000) 750.0 0.001)



;; Structures (Furniture)
(define-type Furniture (U Desk Bookshelves))
(struct Desk ([width : Real] [height : Real] [depth : Real]) #:transparent)
(struct Bookshelves ([depth : Real] [shelves : Integer] [shelves-width : Real]) #:transparent)

(define my-desk (Desk 50 40 30))
(define my-shelf (Bookshelves 20 10 40))

(define (furniture-footprint [my-furniture : Furniture]) : Any
    (match my-furniture
        [(Desk width height depth) (* width depth)]
        [(Bookshelves depth shelves shelves-width) (* shelves-width depth)]))

;; Test Case
(check-equal? (furniture-footprint (Desk 50 40 30)) 1500)
(check-equal? (furniture-footprint (Bookshelves 20 10 40)) 800)