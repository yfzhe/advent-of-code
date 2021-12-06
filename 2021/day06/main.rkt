#lang racket
(require threading)

(module+ test
  (require rackunit))

;; Timer = 0 | 1 | ... | 8

;; parse-input : Input-Port -> (Listof Timer)
(define (parse-input in)
  (~> (read-line in)
      (string-split _ ",")
      (map string->number _)))

(module+ test
  (define input
    (call-with-input-file "test" parse-input)))

;; -------------------- PART 1 & 2 --------------------

;; group fish by their timers:
;; Group = (Dict Timer Int)

;; group-fish : (Listof Timer) -> Group
(define (group-fish fish)
  (for/fold ([acc '()])
            ([f (in-list fish)])
    (dict-update acc f add1 0)))

;; simulate/one : Group -> Group
(define (simulate/one group)
  (for/fold ([acc '()])
            ([(timer cnt) (in-dict group)])
    (cond
      [(zero? timer)
       (~> (dict-update acc 6 (curry + cnt) 0)
           (dict-set _ 8 cnt))]
      [else
       (dict-update acc (sub1 timer) (curry + cnt) 0)])))

;; simulate : (Listof Timer) * Int -> Int
(define (simulate init days)
  (~> (group-fish init)
      (repeat simulate/one _ days)
      dict-values
      (apply + _)))

;; repeat : (a -> a) * a * Int -> a
(define (repeat proc init times)
  (cond
    [(zero? times) init]
    [else (repeat proc (proc init) (sub1 times))]))

(module+ test
  (check-equal? (simulate input 18) 26)
  (check-equal? (simulate input 80) 5934))

(module+ part1
  (~> (call-with-input-file "input" parse-input)
      (simulate _ 80)))

(module+ part2
  (~> (call-with-input-file "input" parse-input)
      (simulate _ 256)))
