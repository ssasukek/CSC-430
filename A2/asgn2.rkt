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
    ;and produces how much income the attendees produce. 








;; Problem 3.3.3
    ; Develop area-cylinder. The program consumes the radius of the cylinder's 
    ; base disk and its height. Its result is the surface area of the cylinder.










;; 2.2 Playing Card Manufacturing 
    ; Develop the function next-card
    ; Accepts a card and return the next higher card that appear in same suit straight 










;; 2.3 Low-degree Polynomial
    ; Develop a data defintion for a polynomial
    ; vairents call Linear and Quadratic











;; 2.4 Derivative
    ; Develop the fucntion derivative
    ; That accepts a polynomial and returns another polynomial representing its derivative
    ; do not return a quadratic polynomial whose first coefficient is zero










;; 2.5 Binary tree
    ; Develop a data definition for a full binary tree called BTree
    ; with symbols at each leaf but no value at node
    ; varient named Leaf and Node
    ; each node has exactly two children
    ; include define-type









;; 2.6 ZZ-Tree
    ; develop the zz-tree function that accepts a binary tree and produces a new binary tree
    ; symbol 'zz









;; 2.7 Mirror
    ; develop the mirror function that accpets abinary tree and produces a new binary tree
    ; left-right mirror image









;; 2.8 Min-Depth
    ; Develop the contains? function that accepts a binary tree and a symbol and return true










;; 2.10 Subsitution
    ; Develop the subst function that accpets a source BTree and a symbol and a replacemnet BTree










;; 2.11 All Path Lengths
    ; develop the all-path-lengths function that accepts a binary tree and returns a lsit containing the legnths of all of the paths
