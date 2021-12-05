#lang racket
(require threading
         aoc-util)

(module+ test
  (require rackunit))

;; Point = (Cons Int Int)
;; Line = (List (Cons Int Int) (Cons Int Int))
;;
;; Vents = (Hash Point Int)

(define *line-rx* #px"(\\d+),(\\d+) -> (\\d+),(\\d+)")

;; parse-input : Input-Port -> (Listof Line)
(define (parse-input in)
  (for/list ([line (in-lines in)])
    (match (regexp-match *line-rx* line)
      [(list _ (num-str x1) (num-str y1) (num-str x2) (num-str y2))
       (list (cons x1 y1) (cons x2 y2))])))

(module+ test
  (define input
    (call-with-input-file "test" parse-input)))

;; -------------------- PART 1 --------------------

;; process-lines : (Line -> (Listof Point) * (Listof Line) -> Vent
(define (process-lines line->points lines)
  (define vents (make-hash))
  (for* ([line (in-list lines)]
         [point (in-list (line->points line))])
    (hash-update! vents point add1 0))
  vents)

;; line->points/1 : Line -> (Listof Point)
(define (line->points/1 line)
  (match-define `((,x1 . ,y1) (,x2 . ,y2)) line)
  (cond
    [(= x1 x2)
     (for/list ([y (in-inclusive-range y1 y2 (sgn (- y2 y1)))])
       (cons x1 y))]

    [(= y1 y2)
     (for/list ([x (in-inclusive-range x1 x2 (sgn (- x2 x1)))])
       (cons x y1))]

    [else '()]))

;; count-overlaps : (Hash Point Int) -> Int
(define (count-overlaps vents)
  (count (lambda (cnt) (>= cnt 2))
         (hash-values vents)))

(module+ test
  (check-equal? (count-overlaps (process-lines line->points/1 input))
                5))

(module+ part1
  (~> (call-with-input-file "input" parse-input)
      (process-lines line->points/1 _)
      count-overlaps))

;; -------------------- PART 2 --------------------

;; line->points/2 : Line -> (Listof Point)
(define (line->points/2 line)
  (match-define `((,x1 . ,y1) (,x2 . ,y2)) line)
  (cond
    [(= (abs (- x1 x2)) (abs (- y1 y2)))
     (for/list ([x (in-inclusive-range x1 x2 (sgn (- x2 x1)))]
                [y (in-inclusive-range y1 y2 (sgn (- y2 y1)))])
       (cons x y))]

    [else (line->points/1 line)]))

(module+ test
  (check-equal? (count-overlaps (process-lines line->points/2 input))
                12))

(module+ part2
  (~> (call-with-input-file "input" parse-input)
      (process-lines line->points/2 _)
      count-overlaps))
