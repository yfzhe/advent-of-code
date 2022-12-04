#lang racket
(require aoc-util)

(module+ test
  (require rackunit))

(define (parse-line line)
  (match (regexp-match #px"(\\d+)-(\\d+),(\\d+)-(\\d+)" line)
    [(list _ nums ...)
     (map string->number nums)]))

(module+ test
  (check-equal? (parse-line "2-4,6-8") '(2 4 6 8)))

;; ---------- PART 1 ----------

(define (contain? a b c d)
  (cond
    [(= a c) #t]
    [(< a c) (>= b d)]
    [(> a c) (<= b d)]))

(module+ test
  (check-equal? (contain? 2 4 1 8) #t)
  (check-equal? (contain? 2 6 4 5) #t)
  (check-equal? (contain? 1 4 1 9) #t)
  (check-equal? (contain? 2 4 3 5) #f)
  (check-equal? (contain? 1 3 5 7) #f)
  (check-equal? (contain? 6 8 2 4) #f))

(module+ part1
 (call-with-input-file "input"
   (lambda (in)
     (for/count ([line (in-lines in)])
       (apply contain? (parse-line line))))))

;; ---------- PART 2 ----------

(define (overlap? a b c d)
  (cond
    [(> a c) (overlap? c d a b)]
    [else (>= b c)]))

(module+ test
  (check-equal? (overlap? 5 7 7 9) #t)
  (check-equal? (overlap? 2 8 3 7) #t)
  (check-equal? (overlap? 2 4 5 6) #f)
  (check-equal? (overlap? 4 7 3 8) #t))

(module+ part2
 (call-with-input-file "input"
   (lambda (in)
     (for/count ([line (in-lines in)])
       (apply overlap? (parse-line line))))))
