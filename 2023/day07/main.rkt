#lang racket
(require threading
         aoc-util
         "order.rkt")

(module+ test
  (require rackunit))

;; a <hand> of cards is represented by a string,
;; and each character of it represents a <card>.

;; parse-input : Input-Port -> (List (cons Hand bid))
(define (parse-input in)
  (for/list ([line (in-lines in)])
    (match-define (list hand bid) (string-split line))
    (cons hand (string->number bid))))

(define (count-cards hand)
  (~> (string->list hand)
      (group-by values _)
      (map length _)
      (sort _ >)))

(define (kind* counts)
  (match counts
    ['(5)       7]
    ['(4 1)     6]
    ['(3 2)     5]
    ['(3 1 1)   4]
    ['(2 2 1)   3]
    ['(2 1 1 1) 2]
    [_          1]))

(define (kind/1 hand)
  (kind* (count-cards hand)))

(module+ test
  (check-equal? (kind/1 "AAAAA") 7)
  (check-equal? (kind/1 "AA8AA") 6))

(define (make-hand-order kind card-order-string)
  (define card-order
    (order-map (order-reverse real-order)
               (lambda (c) (index-of (string->list card-order-string) c))))
  (order-chain
   (order-map real-order kind)
   (order-map (lexicographic-order card-order) string->list)))

(define hand-order
  (make-hand-order kind/1 "AKQJT98765432"))

(module+ test
  (check-equal? (hand-order "33332" "2AAAA") '>)
  (check-equal? (hand-order "KTJJT" "KK677") '<))

(define (total-winnings hands order)
  (define hand<? (<? order))
  (define sorted (sort hands hand<? #:key car))
  (for/sum ([hand+bid sorted] [rank (in-naturals 1)])
    (* (cdr hand+bid) rank)))

(module+ part1
  (~> (call-with-input-file "input" parse-input)
      (total-winnings _ hand-order)))

(define (kind/2 hand)
  (define Js (for/count ([c (in-string hand)]) (eq? c #\J)))
  (define counts (count-cards (string-replace hand "J" "")))
  (kind* (cond
           [(= Js 5) '(5)]
           [else (list-update counts 0 (lambda (n) (+ n Js)))])))

(define hand-order/2
  (make-hand-order kind/2 "AKQT98765432J"))

(module+ test
  (check-equal? (hand-order/2 "JKKK2" "QQQQ2") '<)
  (check-equal? (hand-order/2 "KJKK2" "QQQQ2") '>))

(module+ part2
  (~> (call-with-input-file "input" parse-input)
      (total-winnings _ hand-order/2)))
