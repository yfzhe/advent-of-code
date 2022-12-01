#lang racket
(require threading)

(module+ test
  (require rackunit))

;; ---------- PARSE ----------
(define (parse-input in)
  (~> (port->string in)
      (string-split _ "\n\n")
      (map parse-elf-calories _)))

(define (parse-elf-calories str)
  (~> (string-split str "\n")
      (map string->number _)))

(module+ test
  (define input (call-with-input-file "test" parse-input))
  (check-equal? input
                '((1000 2000 3000)
                  (4000)
                  (5000 6000)
                  (7000 8000 9000)
                  (10000))))

;; ---------- PART 1 ----------
(define (sum lst) (apply + lst))

(define (answer1 elves)
  (~> (map sum elves)
      (apply max _)))

(module+ test
  (check-equal? (answer1 input) 24000))

(module+ part1
  (call-with-input-file "input"
    (lambda~> parse-input answer1)))

;; ---------- PART 2 ----------
(define (answer2 elves)
  (~> (map sum elves)
      (sort _ >)
      (take _ 3)
      sum))

(module+ test
  (check-equal? (answer2 input) 45000))

(module+ part2
  (call-with-input-file "input"
    (lambda~> parse-input answer2)))
