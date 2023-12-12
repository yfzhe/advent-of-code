#lang racket
(require threading)

(module+ test
  (require rackunit))

;; Position = (cons Int Int)
;; Galaxies = (Set Position)
;; Universe = Int * Int * Galaxies
(struct universe (width height galaxies))

;; parse-universe : Input-Port -> Universe
(define (parse-universe in)
  (define lines (port->lines in))
  (define galaxies
    (for*/set ([(line row) (in-indexed (in-list lines))]
               [(char col) (in-indexed (in-string line))]
               #:when (eq? char #\#))
      (cons row col)))
  (universe (string-length (car lines))
            (length lines)
            galaxies))

;; expand-universe* : Universe * Int -> Galaxies
(define (expand-universe* univ factor)
  (define d (- factor 1))
  (define empty-rows (find-empty-rows univ))
  (define empty-cols (find-empty-columns univ))
  (for/set ([pos (in-set (universe-galaxies univ))])
    (match-define (cons row col) pos)
    (cons (+ row (* d (count-less row empty-rows)))
          (+ col (* d (count-less col empty-cols))))))

;; find-empty-rows: Universe -> (Listof Int)
(define (find-empty-rows univ)
  (match-define (universe width height galaxies) univ)
  (for/list ([row (in-range height)]
             #:when
             (for/and ([col (in-range width)])
               (not (set-member? galaxies (cons row col)))))
    row))

;; find-empty-columns: Universe -> (Listof Int)
(define (find-empty-columns univ)
  (match-define (universe width height galaxies) univ)
  (for/list ([col (in-range width)]
             #:when
             (for/and ([row (in-range height)])
               (not (set-member? galaxies (cons row col)))))
    col))

;; count-less : (Listof Number) * Number -> Int
;;   pre-condition: `lst` is a sorted list
;;   return the number of elements which is less then n
(define (count-less n lst)
  (let loop ([acc 0] [lst lst])
    (cond
      [(null? lst) acc]
      [(< (car lst) n) (loop (add1 acc) (cdr lst))]
      [else acc])))

;; manhattan-distance : Position * Position -> Int
(define (manhattan-distance p1 p2)
  (match-define (cons x1 y1) p1)
  (match-define (cons x2 y2) p2)
  (+ (abs (- x1 x2)) (abs (- y1 y2))))

;; sum-of-dists : Galaxies -> Int
(define (sum-of-dists galaxies)
  (for/sum ([pair (in-combinations (set->list galaxies) 2)])
    (apply manhattan-distance pair)))

;; solve : Int -> Input-Port -> Int
(define (solve factor)
  (lambda~> parse-universe
            (expand-universe* _ factor)
            sum-of-dists))

(module+ test
  (check-equal? (call-with-input-file "test" (solve 2)) 374)
  (check-equal? (call-with-input-file "test" (solve 10)) 1030)
  (check-equal? (call-with-input-file "test" (solve 100)) 8410))

(module+ part1
  (call-with-input-file "input" (solve 2)))

(module+ part2
  (call-with-input-file "input" (solve 1000000)))
