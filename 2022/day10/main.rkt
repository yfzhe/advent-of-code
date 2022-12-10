#lang racket
(require threading)

(module+ test
  (require rackunit))

(define (parse-input in)
  (for/list ([line (in-lines in)])
    (parse-op line)))

(define (parse-op line)
  (match (string-split line)
    [(list "noop") '(noop)]
    [(list "addx" num) `(addx ,(string->number num))]))

(module+ test
  (check-equal? (parse-op "noop") '(noop))
  (check-equal? (parse-op "addx 24") '(addx 24))
  (check-equal? (parse-op "addx -10") '(addx -10)))

(define (run-program prog)
  (for/fold ([x 1]
             [history '(1)]
             #:result (reverse history))
            ([op (in-list prog)])
    (match op
      ['(noop)
       (values x (cons x history))]
      [`(addx ,v)
       (define next-x (+ x v))
       (values next-x (cons next-x (cons x history)))])))

(define (signal-strength-sum history)
  (for/sum ([x (in-list history)]
            [cycle (in-naturals 1)]
            #:when (= (remainder cycle 40) 20))
    (* cycle x)))

(module+ part1
  (call-with-input-file "test"
    (lambda~> parse-input
              run-program
              signal-strength-sum)))

(define (display-crt history)
  (for ([sprite (in-list history)]
        [cycle-1 (in-naturals)])
    (define x (remainder cycle-1 40))

    (when (and (> cycle-1 0) (= x 0))
      (newline))

    (display (if (<= (abs (- x sprite)) 1)
                 #\# #\.))))

(module+ part2
 (call-with-input-file "input"
   (lambda~> parse-input
             run-program
             display-crt)))
