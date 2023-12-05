#lang racket

(module+ test
  (require rackunit))

(define (parse-card line)
  (match-define (list _  winners numbers)
    (regexp-match #px"Card +[0-9]+: ([\\d ]+) \\| ([\\d +]+)" line))
  (list
   (map string->number (string-split winners))
   (map string->number (string-split numbers))))

(module+ test
  (define card1 (parse-card "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53"))
  (check-equal? card1 (list '(41 48 83 86 17) '(83 86 6 31 17 9 48 53))))

(define (card-matches card)
  (match-define (list winners numbers) card)
  (count (lambda (n) (member n winners)) numbers))

(module+ test
  (check-equal? (card-matches card1) 4))

(module+ part1
  (call-with-input-file "input"
    (lambda (in)
      (for/sum ([line (in-lines in)])
        (define card (parse-card line))
        (define matches (card-matches card))
        (if (zero? matches) 0 (expt 2 (sub1 matches)))))))

(module+ part2
  (call-with-input-file "test"
    (lambda (in)
      (define matches
        (for/list ([line (in-lines in)])
          (card-matches (parse-card line))))

      (for*/fold ([acc (make-list (length matches) 1)])
                 ([(m i) (in-indexed (in-list matches))]
                  #:do [(define n (list-ref acc i))]
                  [j (in-range (+ i 1) (+ i m 1))])
        (list-update acc j (lambda (k) (+ k n)))))))
