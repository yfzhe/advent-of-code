#lang racket
(require threading)

(module+ test
  (require rackunit))

;; Digit = #\0 | #\1
;; Num = (Listof Digit)

;; parse-input : Input-Port -> (Listof Num)
(define (parse-input in)
  (for/list ([line (in-lines in)])
    (string->list line)))

(module+ test
  (define input
    (call-with-input-file "test" parse-input)))

;; ->number : Num -> Number
(define (->number num)
  (~> num
      list->string
      (string->number _ 2)))

;; -------------------- PART 1 --------------------

;; most-common : Digit ... -> Digit
(define (most-common-digit . digits)
  (define zeros (count (curry equal? #\0) digits))
  (define ones (- (length digits) zeros))
  (cond
    [(> zeros ones) #\0]
    [else #\1]))

;; negate : Digit -> Digit
(define (negate digit)
  (case digit
    [(#\0) #\1]
    [(#\1) #\0]))

;; less-common-digit : Digit ... -> Digit
(define (less-common-digit . digits)
  (~> (apply most-common-digit digits)
      negate))

;; gamma-rate : (Listof Num) -> Number
(define (gamma-rate nums)
  (~> (apply map most-common-digit nums)
      ->number))

;; epsilon-rate : (Listof Num) -> Number
(define (epsilon-rate nums)
  (~> (apply map less-common-digit nums)
      ->number))

(module+ test
  (check-equal? (gamma-rate input) #b10110)
  (check-equal? (epsilon-rate input) #b01001))

(module+ part1
  (define input
    (call-with-input-file "input" parse-input))
  (* (gamma-rate input)
     (epsilon-rate input)))


;; -------------------- PART 2 --------------------

;; find-rating : (Digit ... -> Digit) * (Listof Num) -> Num
(define (find-rating finder nums)
  (let loop ([nums nums]
             [idx 0])
    (define digits
      (map (lambda (n) (list-ref n idx)) nums))
    (define bit-criteria
      (apply finder digits))
    (define remain
      (filter (lambda (n) (equal? bit-criteria (list-ref n idx)))
              nums))
    (cond
      [(= (length remain) 1)
       (car remain)]
      [else
       (loop remain (add1 idx))])))

;; oxygen-generator-rating : (Listof Num) -> Number
(define (oxygen-generator-rating nums)
  (~> (find-rating most-common-digit nums)
      ->number))

;; co2-scrubber-rating : (Listof Num) -> Number
(define (co2-scrubber-rating nums)
  (~> (find-rating less-common-digit nums)
      ->number))

(module+ test
  (check-equal? (oxygen-generator-rating input) #b10111)
  (check-equal? (co2-scrubber-rating input) #b01010))

(module+ part2
  (define input
    (call-with-input-file "input" parse-input))
  (* (oxygen-generator-rating input)
     (co2-scrubber-rating input)))
