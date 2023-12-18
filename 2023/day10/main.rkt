#lang racket

(module+ test
  (require rackunit))

;; Point = (cons Int Int)
;; Graph = (Hash Point (Listof Point))
;; Direction = (cons Int Int)

(define up    '(-1 . 0))
(define down  '(1 . 0))
(define left  '(0 . -1))
(define right '(0 . 1))

;; apply-direction : Direction * Point -> Point
(define (apply-direction dir p)
  (cons (+ (car p) (car dir))
        (+ (cdr p) (cdr dir))))

;; parse-input : Input-Port -> Graph
(define (parse-input in)
  (for*/hash ([(line row) (in-indexed (in-lines in))]
              [(char col) (in-indexed (in-string line))]
              #:unless (eq? char #\.))
    (define point (cons row col))
    (values point
            (map (lambda (dir) (apply-direction dir point))
                 (next-directions char)))))

;; next-directions : Char -> (Listof Direction)
(define (next-directions char)
  (match char
    [#\| (list up down)]
    [#\- (list left right)]
    [#\L (list up right)]
    [#\J (list up left)]
    [#\7 (list left down)]
    [#\F (list right down)]
    [#\S '()]))

;; graph-start-point : Graph -> Point
(define (graph-start-point graph)
  (for/first ([(point linked) (in-hash graph)]
              #:when (null? linked))
    point))

;; try-loop : Graph * Point * Point -> (Listof Point)
(define (try-loop graph start second)
  (let loop ([last start] [cur second] [acc (list start)])
    (cond
      [(not (hash-has-key? graph cur)) '()]
      [(equal? cur start) (reverse acc)]
      [else
       (define linked (hash-ref graph cur))
       (define next (car (remove last linked)))
       (loop cur next (cons cur acc))])))

;; find-loop : Graph -> (Listof Point)
(define (find-loop graph)
  (define start (graph-start-point graph))
  (for/first ([dir (list up down left right)]
              #:do [(define candidate (apply-direction dir start))]
              #:when (member start (hash-ref graph candidate '()))
              #:do [(define loop (try-loop graph start candidate))]
              #:unless (null? loop))
    loop))

(module+ test
  (define graph (call-with-input-file "test" parse-input))
  (check-equal? (length (find-loop graph)) 16))

(module+ part1
  (define graph (call-with-input-file "input" parse-input))
  (define loop (find-loop graph))
  (exact-ceiling (/ (- (length loop) 1) 2)))
