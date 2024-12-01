#lang racket

;; parse-input : Input-Port -> (Listof Number) * (Listof Number)
(define (parse-input in)
  (define numbers
    (for/list ([line (in-lines in)])
      (map string->number (string-split line))))

  (values (map first numbers)
          (map second numbers)))

(module+ part1
  (define-values (left right)
    (call-with-input-file "input" parse-input))

  (for/sum ([l (in-list (sort left <))]
            [r (in-list (sort right <))])
    (abs (- l r))))

;; count : (Listof Number) -> (Hash Number Integer)
(define (count lst)
  (define acc (make-hash))
  (for ([num (in-list lst)])
    (hash-update! acc num add1 0))
  acc)

(module+ part2
  (define-values (left right)
    (call-with-input-file "input" parse-input))

  (define counts (count right))
  (for/sum ([l (in-list left)])
    (* l (hash-ref counts l 0))))
