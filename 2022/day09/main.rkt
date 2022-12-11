#lang racket
(require threading)

(module+ test
  (require rackunit))

(define (parse-input in)
  (for/list ([line (in-lines in)])
    (list (string-ref line 0)
          (string->number (substring line 2)))))

;; pos ::= (cons <number> <number>)

(define (posmap proc . pos-list)
  (cons (apply proc (map car pos-list))
        (apply proc (map cdr pos-list))))

(define (dir->move dir)
  (match dir
    [#\R '(1 . 0)]
    [#\L '(-1 . 0)]
    [#\U '(0 . 1)]
    [#\D '(0 . -1)]))

(define (simulate motions knots-count)
  (for*/fold ([knots (make-list knots-count '(0 . 0))]
              [tail-visited (set '(0 . 0))]
              #:result tail-visited)
             ([motion (in-list motions)]
              #:do [(match-define (list head-dir steps) motion)]
              [i (in-range steps)])
    (match-define (cons head tails) knots)
    (define next-head
      (posmap + head (dir->move head-dir)))

    (define next-knots
      (for/fold ([acc (list next-head)]
                 #:result (reverse acc))
                ([tail (in-list tails)])
        (define head* (car acc))
        (define dist (posmap - head* tail))
        (define tail-move
          (if (and (<= (abs (car dist)) 1)
                   (<= (abs (cdr dist)) 1))
              '(0 . 0)
              (posmap sgn dist)))
        (define next-tail (posmap + tail tail-move))
        (cons next-tail acc)))

    (values next-knots
            (set-add tail-visited (last next-knots)))))

(define (answer filename knots-count)
  (call-with-input-file filename
    (lambda~> parse-input
              (simulate _ knots-count)
              set-count)))

(module+ test
  (check-equal? (answer "test" 2) 13)
  (check-equal? (answer "test2" 10) 36))

(module+ part1
  (answer "input" 2))

(module+ part2
  (answer "input" 10))
