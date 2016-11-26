(ns chp1.core)
(require 'clojure.core)

;; exercise 1
;; return the second element of a list
(def second (fn [list] (first (rest list))))

;; exercise 2
;; give 2 implementation of third
(def third (fn [list] (nth list 2)))

;; another implementation of third
(def third-2 (fn [list] (second (rest list))))


;; exercise 3
;; sum of squares
(def add-squares (fn [& list]
    (apply + (map (fn [n] (* n n)) list))
))

;; exercise 4
;; use range for factorial
(def factorial (fn [n]
    (apply * (range 1 (+ n 1)))    
))

;; exercise 5
;; obtain a list with only distinct elements when provided with nested lists
(def nested-distinct (fn [list]
    (distinct (flatten list))    
))

;; given a list and a number, return the list repeated the number times
(def repeat-list (fn [list n]
    (apply concat (repeat n list))    
))

;; find the product of correspondingly indexed elements
(def product (fn [l1 l2]
    (map (fn [l]
            (apply * l)) (partition 2 (interleave l1 l2)))
))

;; check if every element is odd
(def all-odd? (fn [list]
    (every? odd? list)))

;; remove all nil values from a sequence
(def remove-nils (fn [l]
    (remove nil? l)    
))

;; exercise 6
(def prefix-of? (fn [candidate sequence]
    (= candidate (take (count candidate) sequence))    
))