#lang racket/base
(require racket/match racket/list)

(provide (all-defined-out))

;; influenced by rebellion's comparators:
;; - https://docs.racket-lang.org/rebellion/Comparators.html
;; see also:
;; - https://docs.racket-lang.org/data/Orders_and_Ordered_Dictionaries.html
;; - https://srfi.schemers.org/srfi-67/srfi-67.html

;; An "order" compares two values and return '<, '= or '>
;; (Order a) = a * a -> (U '< '= '>)

;; real-order : (Order Real)
(define real-order
  (lambda (x y)
    (cond
      [(< x y) '<] [(= x y) '=] [else '>])))

;; enum-order : (List e) -> (Order e)
(define (enum-order enum-cases)
  (order-map real-order
             (lambda (e) (index-of enum-cases e))))

;; order-map : (Order b) * (a -> b) -> (Order a)
(define (order-map order f)
  (lambda (x y)
    (order (f x) (f y))))

;; order-chain : (Order a) * (Order a) -> (Order a)
(define (order-chain ord1 ord2)
  (lambda (x y)
    (match (ord1 x y)
      ['< '<]
      ['= (ord2 x y)]
      ['> '>])))

;; lexicographic-order : (Order a) -> (Order (List a))
(define (lexicographic-order order)
  (letrec ([cmp
            (lambda (xs ys)
              (match* (xs ys)
                [('() '()) '=]
                [('()  _ ) '<]
                [(_   '()) '>]
                [((cons x xs*) (cons y ys*))
                 (match (order x y)
                   ['< '<]
                   ['= (cmp xs* ys*)]
                   ['> '>])]))])
    cmp))

;; <? : (Order a) -> (a * a -> Boolean)
(define (<? order)
  (lambda (x y)
    (eq? (order x y) '<)))

;; =? : (Order a) -> (a * a -> Boolean)
(define (=? order)
  (lambda (x y)
    (eq? (order x y) '=)))

;; >? : (Order a) -> (a * a -> Boolean)
(define (>? order)
  (lambda (x y)
    (eq? (order x y) '>)))
