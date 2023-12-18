#lang racket
(require aoc-util)

(module+ test
  (require rackunit))

;; Point = (cons Int Int)
;; Direction = (cons Int Int)
(define up    '(-1 . 0))
(define down  '(1 . 0))
(define left  '(0 . -1))
(define right '(0 . 1))

;; apply-direction : Direction * Point -> Point
(define (apply-direction dir p)
  (cons (+ (car p) (car dir))
        (+ (cdr p) (cdr dir))))

;; Sketch = Int * Int * Graph
;; Graph = (Hash Point (Listof Point))
(struct sketch (width height graph))

;; parse-input : Input-Port -> Sketch
(define (parse-input in)
  (define lines (port->lines in))
  (define graph
    (for*/hash ([(line row) (in-indexed (in-list lines))]
                [(char col) (in-indexed (in-string line))]
                #:unless (eq? char #\.))
      (define point (cons row col))
      (values point
              (map (lambda (dir) (apply-direction dir point))
                   (next-directions char)))))
  (sketch (string-length (car lines))
          (length lines)
          graph))

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

;; ---------- PART 1 ----------
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

;; find-loop : Sketch -> (Listof Point)
(define (find-loop sketch)
  (define graph (sketch-graph sketch))
  (define start (graph-start-point graph))
  (for/first ([dir (list up down left right)]
              #:do [(define candidate (apply-direction dir start))]
              #:when (member start (hash-ref graph candidate '()))
              #:do [(define loop (try-loop graph start candidate))]
              #:unless (null? loop))
    loop))

(module+ test
  (define sketch1 (call-with-input-file "test" parse-input))
  (check-equal? (length (find-loop sketch1)) 16))

(module+ parts
  (define sketch (call-with-input-file "input" parse-input))
  (define loop (find-loop sketch))
  (exact-ceiling (/ (- (length loop) 1) 2)))

;; ---------- PART 2 ----------

;; VLine = (List x y1 y2) where y1 < y2

;; vertical-lines : (Listof Point) -> (Listof VLine)
(define (vertical-lines points)
  (define points* (cons (last points) points))
  (let loop ([ps points*] [acc '()])
    (match ps
      [(list p) acc]
      [(list (cons x1 y1) (cons x2 y2) more ...)
       (loop (cdr ps)
             (cond
               [(not (= x1 x2)) acc]
               [(< y1 y2)       (cons (list x1 y1 y2) acc)]
               [else            (cons (list x1 y2 y1) acc)]))])))

;; tiles-inside-loop : Sketch * (Listof Point) -> Int
(define (tiles-inside-loop sketch loop)
  (define points* (list->set loop))

  (define vlines (vertical-lines loop))
  (define vlines* (sort vlines < #:key first))

  (for*/count ([x (in-range (sketch-width sketch))]
               [y (in-range (sketch-height sketch))])
    (and (not (set-member? points* (cons x y)))
         (odd?
          (for/count ([line (in-list vlines*)]
                      #:do [(match-define (list xl y1 y2) line)]
                      #:break (>= xl x))
            (and (< y1 y) (<= y y2)))))))

(module+ test
  (define sketch2 (call-with-input-file "test2" parse-input))
  (check-equal? (tiles-inside-loop sketch2 (find-loop sketch2)) 4))

(module+ parts
  (tiles-inside-loop sketch loop))
