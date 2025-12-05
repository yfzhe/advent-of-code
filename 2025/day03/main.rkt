#lang racket
(require threading)

(module+ test
  (require rackunit))

(define (parse-line line)
  (~> (string->list line)
      (map char->digit _)))

(define (char->digit char)
  (- (char->integer char) (char->integer #\0)))

(define (digits->number digits)
  (foldl (lambda (d acc) (+ (* acc 10) d)) 0 digits))

;; brute-force
(define (solve/1 line)
  (~> (parse-line line)
      (combinations _ 2)
      (map digits->number _)
      (apply max _)))

(module+ test
  (check-equal? (solve/1 "987654321111111") 98)
  (check-equal? (solve/1 "811111111111119") 89)
  (check-equal? (solve/1 "234234234234278") 78)
  (check-equal? (solve/1 "818181911112111") 92))

(module+ part1
  (call-with-input-file "input"
    (lambda (in)
      (for/sum ([line (in-lines in)])
        (solve/1 line)))))

;; greedy
(define (solve/2 line k)
  (solve* (parse-line line) k '()))

(define (solve* nums k acc)
  (cond
    [(zero? k)
     (digits->number (reverse acc))]
    [else
     (define l (length nums))
     (define-values (max max-at)
       (for/fold ([max 0] [max-at 0])
                 ([d (in-list nums)]
                  [i (in-inclusive-range 0 (- l k))])
         (if (> d max)
             (values d i)
             (values max max-at))))
     (solve* (drop nums (add1 max-at))
             (sub1 k)
             (cons max acc))]))

(module+ test
  (check-equal? (solve/2 "987654321111111" 2) 98)
  (check-equal? (solve/2 "811111111111119" 2) 89)
  (check-equal? (solve/2 "234234234234278" 2) 78)
  (check-equal? (solve/2 "818181911112111" 2) 92)

  (check-equal? (solve/2 "987654321111111" 12) 987654321111)
  (check-equal? (solve/2 "811111111111119" 12) 811111111119)
  (check-equal? (solve/2 "234234234234278" 12) 434234234278)
  (check-equal? (solve/2 "818181911112111" 12) 888911112111))

(module+ part2
  (call-with-input-file "input"
    (lambda (in)
      (for/sum ([line (in-lines in)])
        (solve/2 line 12)))))
