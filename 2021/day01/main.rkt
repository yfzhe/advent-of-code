#lang racket
(require threading)

(module+ test
  (require rackunit))

;; parse-input : Input-Port -> (Listof Int)
(define (parse-input in)
  (for/list ([line (in-lines in)])
    (string->number line)))

(module+ test
  (define input
    (call-with-input-file "test" parse-input)))

;;; -------------------- Part 1 --------------------

;; count-inc : (Listof Int) -> Int
(define (count-inc nums)
  (for/fold ([acc 0])
            ([num1 (in-list nums)]
             [num2 (in-list (cdr nums))])
    (if (> num2 num1)
        (add1 acc)
        acc)))

(module+ test
  (check-equal? (count-inc input) 7))

(module+ part1
  (call-with-input-file "input"
    (lambda~> parse-input
              count-inc)))

;;; -------------------- Part 2 --------------------

;; fold-3 : (Listof Int) -> (Listof Int)
(define (fold-3 nums)
  (for/list ([num1 (in-list nums)]
             [num2 (in-list (cdr nums))]
             [num3 (in-list (cddr nums))])
    (+ num1 num2 num3)))

(module+ test
  (check-equal? (count-inc (fold-3 input))
                5))

(module+ part2
  (call-with-input-file "input"
    (lambda~> parse-input
              fold-3
              count-inc)))
