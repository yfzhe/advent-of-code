#lang racket
(require threading
         aoc-util)

(module+ test
  (require rackunit))

;; Card = char?
;; Hand = string?

;; parse-input : Input-Port -> (List (cons Hand bid))
(define (parse-input in)
  (for/list ([line (in-lines in)])
    (match-define (list hand bid) (string-split line))
    (cons hand (string->number bid))))

(define (kind hand)
  (~> (string->list hand)
      (group-by values _)
      (map length _)
      (sort _ >)
      kind*))

(define (kind* dist)
  (match dist
    [(list 5)       7]
    [(list 4 1)     6]
    [(list 3 2)     5]
    [(list 3 1 1)   4]
    [(list 2 2 1)   3]
    [(list 2 1 1 1) 2]
    [_              1]))

(module+ test
  (check-equal? (kind "AAAAA") 7)
  (check-equal? (kind "AA8AA") 6))

(define card=? eq?)

(define (card>? card1 card2)
  (define (idx c)
    (index-of (string->list "AKQJT98765432") c))
  (< (idx card1) (idx card2)))

(define (lexicographic>? >? =? as bs)
  (match* (as bs)
    [('() '()) #f]
    [('() _) #t]
    [((cons a as) (cons b bs))
     (cond
       [(>? a b) #t]
       [(=? a b) (lexicographic>? >? =? as bs)]
       [else #f])]))

(define (hand>?/1 hand1 hand2)
  (define dk (- (kind hand1) (kind hand2)))
  (or (> dk 0)
      (and (= dk 0)
           (lexicographic>? card>? card=?
                            (string->list hand1) (string->list hand2)))))

(module+ test
  (check-equal? (hand>?/1 "33332" "2AAAA") #t)
  (check-equal? (hand>?/1 "KTJJT" "KK677") #f))

(define (total-winnings hands hand>?)
  (define sorted (reverse (sort hands hand>? #:key car)))
  (for/sum ([hand+bid sorted] [rank (in-naturals 1)])
    (* (cdr hand+bid) rank)))

(module+ part1
  (~> (call-with-input-file "input" parse-input)
      (total-winnings _ hand>?/1)))

(define (idx/2 c)
  (index-of (string->list "AKQT98765432J") c))

(define (card>?/2 card1 card2)
  (< (idx/2 card1) (idx/2 card2)))

(define (kind/2 hand)
  (define Js
    (for/count ([char (in-string hand)]) (card=? char #\J)))
  (define dist
    (~> (string-replace hand "J" "")
        string->list
        (group-by values _)
        (map length _)
        (sort _ >)))
  (kind* (cond
           [(= Js 5) (list 5)]
           [else (list-update dist 0 (lambda (n) (+ n Js)))])))

(define (hand>?/2 hand1 hand2)
  (define dk (- (kind/2 hand1) (kind/2 hand2)))
  (or (> dk 0)
      (and (= dk 0)
           (lexicographic>? card>?/2 card=?
                            (string->list hand1) (string->list hand2)))))

(module+ test
  (check-equal? (hand>?/2 "JKKK2" "QQQQ2") #f)
  (check-equal? (hand>?/2 "KJKK2" "QQQQ2") #t))

(module+ part2
  (~> (call-with-input-file "input" parse-input)
      (total-winnings _ hand>?/2)))
