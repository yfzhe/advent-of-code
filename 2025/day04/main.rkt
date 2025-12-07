#lang racket

(module+ test
  (require rackunit))

;; Position: (Int . Int)
;; Grid: a set of Pos, recording only the positions "@" (roll of paper)

;; parse-input : Input-Port -> Grid
(define (parse-input in)
  (for*/set ([(line row) (in-indexed (in-lines in))]
             [(char col) (in-indexed (in-string line))]
             #:when (equal? char #\@))
    (cons row col)))

(define (accessible? grid pos)
  (match-define (cons row col) pos)
  (define cnt
    (for*/sum ([r (in-inclusive-range (- row 1) (+ row 1))]
               [c (in-inclusive-range (- col 1) (+ col 1))]
               #:when (set-member? grid (cons r c)))
      1))
  (< cnt 5))

;; find-accessibles : Grid -> (Setof Pos)
(define (find-accessibles grid)
  (for/set ([pos (in-set grid)]
            #:when (accessible? grid pos))
    pos))

(module+ test
  (define input
    (call-with-input-file "test" parse-input))

  (check-equal? (set-count (find-accessibles input))
                13))

(module+ main
  (define grid
    (call-with-input-file "input" parse-input))

  (set-count (find-accessibles grid)))

;; remove-accessibles : Grid -> Grid
(define (remove-accessibles grid)
  (let loop ([grid grid])
    (define accessibles (find-accessibles grid))
    (cond
      [(set-empty? accessibles) grid]
      [else
       (loop (set-subtract grid accessibles))])))

(define (count-accessibles-removed grid)
  (- (set-count grid)
     (set-count (remove-accessibles grid))))

(module+ test
  (check-equal? (count-accessibles-removed input) 43))

(module+ main
  (count-accessibles-removed grid))
