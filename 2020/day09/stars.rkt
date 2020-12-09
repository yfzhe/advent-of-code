#lang racket

(module+ test
  (require rackunit))

;;; n-sum : (Listof Num) * Num * Nat -> (U (Listof Int) #f)
;;;  copy from mbutterick's solution:
;;;  https://github.com/mbutterick/aoc-racket/blob/61e5bf7d9c6c23fd67d5133ad06fb18b3e7966c6/2020/09.rkt#L12
(define (n-sum nums target n)
  (for/or ([lst (in-combinations nums n)])
    (and (= (apply + lst) target)
         lst)))

(define (two-sum nums target) (n-sum nums target 2))

;;; parse-input : Input-Port -> (Listof Int)
(define (parse-input in)
  (for/list ([line (in-lines in)])
    (string->number line)))

;;; ---------------------- star 1 --------------------------

;;; first-invalid : Nat * (Listof Num) -> Num
(define (first-invalid length-of-preamble nums)
  (let loop ([nums nums])
    (define-values (base remain)
      (split-at nums length-of-preamble))
    (define target (car remain))
    (if (two-sum base target)
        (loop (cdr nums))
        target)))

(module+ test
  (define lst '(35
                20
                15
                25
                47
                40
                62
                55
                65
                95
                102
                117
                150
                182
                127
                219
                299
                277
                309
                576))
  (check-equal? (first-invalid 5 lst) 127))

(module+ stars
  (define input
    (call-with-input-file "input.txt" parse-input))
  (define target (first-invalid 25 input))
  (displayln target))

;;; ---------------------- star 2 --------------------------

;;; continuous-nums : (Listof Num) * Num -> (U (Listof Num) #f)
(define (continuous-nums nums target)
  (let loop ([nums nums])
    (cond
      [(null? nums) #f]
      [else
       (or (try-once nums target)
           (loop (cdr nums)))])))

;;; try-once : (Listof Num) * Num -> (U (Listof Num) #f)
(define (try-once nums target)
  (let loop ([acc null]
             [sum 0]
             [remain nums])
    (cond
      [(null? remain) #f]
      [(> sum target) #f]
      [(= sum target) (reverse acc)]
      [else
       (let ([num (car remain)])
         (loop (cons num acc)
               (+ num sum)
               (cdr remain)))])))

(module+ test
  (check-equal? (try-once lst 127) #f)
  (check-equal? (try-once (cddr lst) 127) '(15 25 47 40))

  (check-equal? (continuous-nums lst 127) '(15 25 47 40)))

(module+ stars
  (define group
    (continuous-nums input target))
  (+ (apply min group)
     (apply max group)))
