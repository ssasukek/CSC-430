#lang typed/racket
(require typed/rackunit)

;; Test case usage: (check-equal? ...) or (check-= ...)
;; No Mutation
;; Use Real Numbers


;; 2.1 Textbook Problems

;; Problem 2.3.3
    ; An old-style movie theater has a simple profit function. 
    ; Each customer pays $5 per ticket. Every performance costs 
    ; the theater $20, plus $.50 per attendee. Develop the function 
    ; total-profit. It consumes the number of attendees (of a show) 
    ; and produces how much income the attendees produce. 

; calculation for total profit : (5 * attendee) - (20 + (.5 * attendee))
(define (total-profit [attendee : Real]) : Real
    (- (* 5 attendee) (+ 20 (* 0.5 attendee))))

(check-= (total-profit 0) -20 0.01)
(check-= (total-profit 10) 25 0.01)
(check-= (total-profit 20) 70 0.01)
(check-= (total-profit 30) 115 0.01)


;; Problem 3.3.3
    ; Develop area-cylinder. The program consumes the radius of the cylinder's 
    ; base disk and its height. Its result is the surface area of the cylinder.

; formula A : (2 pi r h) + (2 pi r^2)
(define (area-cylinder [radius : Real] [height : Real]) : Real
    (+ (* 2 pi radius height)
        (* 2 pi (sqr radius))))

(check-= (area-cylinder 1 1) 12.56 0.01)
(check-= (area-cylinder 0 0) 0 0.01)
(check-= (area-cylinder 5 3) 251.32 0.01)


;; 2.2 Playing Card Manufacturing 
    ; Develop the function next-card
    ; Accepts a card and return the next higher card that appear in same suit straight 
    ; straight -> 1-10, J, Q, K, repeat

; '... is a symbol
(define-type Suit (U 'club 'diamond 'heart 'spade))
(define-type Kind (U 'jack 'queen 'king))
(struct Numeric ([suit : Suit] [pips : Real]) #:transparent)
(struct Facecard ([suit : Suit] [kind : Kind]) #:transparent)

(define-type Card (U Numeric Facecard))
(define (next-card [card : Card]) : Card
    (match card
        ; increment from 1-10
        [(Numeric suit pips)
            (if (< pips 10)     ; pips < 10
                (Numeric suit (+ pips 1))
            (Facecard suit 'jack))]     ; else

        [(Facecard suit 'jack) (Facecard suit 'queen)]
        [(Facecard suit 'queen) (Facecard suit 'king)]
        [(Facecard suit 'king) (Numeric suit 1)]))

(check-equal? (next-card (Numeric 'club 1)) (Numeric 'club 2))
(check-equal? (next-card (Numeric 'diamond 10)) (Facecard 'diamond 'jack))
(check-equal? (next-card (Facecard 'heart 'jack)) (Facecard 'heart 'queen))
(check-equal? (next-card (Facecard 'spade 'queen)) (Facecard 'spade 'king))
(check-equal? (next-card (Facecard 'spade 'king)) (Numeric 'spade 1))


;; 2.3 Low-degree Polynomial
    ; Develop a data defintion for a polynomial
    ; vairents call Linear and Quadratic
    ; linear (Ax + B)   ; quadratic (Ax^2 + Bx + C)

(define-type Polynomial (U Linear Quadratic))
(struct Linear ([a : Real] [b : Real]) #:transparent)
(struct Quadratic ([a : Real] [b : Real] [c : Real]) #:transparent)

(define (interp [polynomial : Polynomial] [x : Real]) : Real
    (match polynomial
    ; Linear
        [(Linear a b)
            (+ (* a x) b)]
        [(Quadratic a b c)
            (+ (* a (sqr x)) (* b x) c)]))

(check-= (interp (Linear 1 2) 3) 5 0.01)
(check-= (interp (Quadratic 1 2 3) 4) 27 0.01)
(check-= (interp (Quadratic 0 0 0) 1) 0 0.01)
(check-= (interp (Linear 0 0) 1) 0 0.01)
(check-= (interp (Quadratic 3 2 -1) 1) 4 0.01)



;; 2.4 Derivative
    ; Develop the fucntion derivative
    ; That accepts a polynomial and returns another polynomial representing its derivative
    ; do not return a quadratic polynomial whose first coefficient is zero
    ; Recap : derivative of x^2 is 2x
        ; linear : ax + b -> a
        ; quad : ax^2 + bx + c -> 2ax + b

(define (derivative [polynomial : Polynomial]) : Polynomial
    (match polynomial
        [(Linear a b)
            (Linear 0 a)]
        [(Quadratic a b c)
            (Linear (* 2 a) b)]))
    
(check-equal? (derivative (Linear 1 2)) (Linear 0 1))
(check-equal? (derivative (Linear 8 3)) (Linear 0 8))
(check-equal? (derivative (Quadratic 1 2 3)) (Linear 2 2))
(check-equal? (derivative (Quadratic 3 3 3)) (Linear 6 3))



;; 2.5 Binary tree
    ; Develop a data definition for a full binary tree called BTree
    ; with symbols at each leaf but no value at node
    ; varient named Leaf and Node
    ; each node has exactly two children
    ; include define-type

(struct Leaf ([val : Symbol]) #:transparent)
(struct Node ([left : BTree] [right : BTree]) #:transparent)
(define-type BTree (U Node Leaf))

; ex 1:
(define tree1 (Leaf 'a))

; ex 2:
(define tree2 (Node (Leaf 'a) (Leaf 'b)))

; ex 3:
(define tree3 (Node (Leaf 'a) (Node (Leaf 'b) (Leaf 'c))))

; ex 4:
(define tree4 (Node 
                (Node (Leaf 'a) 
                    (Node (Leaf 'b) (Leaf 'c)))
                (Node (Leaf 'd) (Leaf 'e))))

;; 2.6 ZZ-Tree
    ; develop the zz-tree function that accepts a binary tree and produces a new binary tree
    ; symbol 'zz

(define (zz-tree [tree : BTree]) : BTree
    (match tree
        [(Leaf _)
            (Leaf 'zz)]
        [(Node left right)
            (Node (zz-tree left) (zz-tree right))]))

(check-equal? (zz-tree (Leaf 'a)) (Leaf 'zz))
(check-equal? (zz-tree (Node (Leaf 'a) (Leaf 'b))) (Node (Leaf 'zz) (Leaf 'zz)))



;; 2.7 Mirror
    ; develop the mirror function that accpets abinary tree and produces a new binary tree
    ; left-right mirror image
    ; input tree 'horse at far left -> new symbol 'horse at far right of tree
    ; swap left with right branch

(define (mirror [tree : BTree]) : BTree
    (match tree
        [(Leaf base)
            (Leaf base)]
        [(Node left right)
            (Node (mirror right) (mirror left))]))      ; swap right with left child

(check-equal? (mirror (Node (Leaf 'a) (Leaf 'b))) (Node (Leaf 'b) (Leaf 'a)))
(check-equal? (mirror (Node (Leaf 'a) (Node (Leaf 'b) (Leaf 'c)))) (Node (Node (Leaf 'c) (Leaf 'b)) (Leaf 'a)))
(check-equal? (mirror (Node (Node (Leaf 'a) (Node (Leaf 'b) (Leaf 'c))) (Node (Leaf 'd) (Leaf 'e)))) 
                (Node (Node (Leaf 'e) (Leaf 'd)) (Node (Node (Leaf 'c) (Leaf 'b)) (Leaf 'a))))



;; 2.8 Min-Depth
    ; develop the min-depth function that accepts a binary tree 
    ; produces the length of the shortest path to a leaf
    ; A single leaf has a min-depth of zero.

(define (min-depth [tree : BTree]) : Real
    (match tree
        [(Leaf _)
            0]      ; single leaf dist -> 0

        ; Algorithm (DFS): Recursively finds min depth of left and right subtrees, taking min(left, right) + 1
        [(Node left right)
            (+ 1 (min (min-depth left) (min-depth right)))]))

(check-equal? (min-depth (Leaf 'a)) 0)
(check-equal? (min-depth (Node (Leaf 'a) (Leaf 'b))) 1)
(check-equal? (min-depth (Node (Node (Leaf 'a) (Node (Leaf 'b) (Leaf 'c))) (Node (Leaf 'd) (Leaf 'e)))) 2)


;; 2.10 Subsitution
    ; Develop the subst function that accpets a source BTree and a symbol and a replacemnet BTree
    ; source tree first arg
    ; return a new tree where every leaf of source containg symbol is replaced
(define (subst [tree : BTree] [symbol : Symbol]) : BTree
    (match tree
        [(Leaf _)
            (Leaf symbol)]
        [(Node left right)
            (Node (subst left symbol) (subst right symbol))]))

(check-equal? (subst (Leaf 'a) 'b) (Leaf 'b))
(check-equal? (subst (Node (Leaf 'a) (Leaf 'b)) 'b) (Node (Leaf 'b) (Leaf 'b)))
(check-equal? (subst (Node (Node (Leaf 'a) (Node (Leaf 'b) (Leaf 'c))) (Node (Leaf 'd) (Leaf 'e))) 'b)
                (Node (Node (Leaf 'b) (Node (Leaf 'b) (Leaf 'b))) (Node (Leaf 'b) (Leaf 'b))))



;; 2.11 All Path Lengths
    ; develop the all-path-lengths function that accepts a binary tree and returns a lsit 
    ; containing the legnths of all of the paths


; (define (all-path-lengths [tree : BTree]) : List
;     (match tree
;         []))