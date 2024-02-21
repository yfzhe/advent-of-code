#lang racket
(require threading
         aoc-util)

(module+ test
  (require rackunit))

;; Hailstone = (List x y z vx vy vz)

;; parse-input : Input-Port -> (Listof Hailstone)
(define (parse-input in)
  (for/list ([line (in-lines in)])
    (parse-hailstone line)))

;; parse-hailstone : String -> Hailstone
(define (parse-hailstone line)
  (~> (string-split line #rx"[, @]+")
      (map string->number _)))

(module+ test
  (define hs (call-with-input-file "test" parse-input)))

;; ---------- PART 1 ----------

;; cross-in? : Hailstone * Hailstone * Int * Int -> Boolean
(define (cross-in? h1 h2 low high)
  (match-define (list x1 y1 _ vx1 vy1 _) h1)
  (match-define (list x2 y2 _ vx2 vy2 _) h2)

  (define d (- (* vx1 vy2) (* vy1 vx2)))
  (cond
    [(zero? d) #f]
    [else
     (define t1
       (/ (- (* (- y1 y2) vx2) (* (- x1 x2) vy2)) d))
     (define t2
       (/ (+ (- x1 x2) (* t1 vx1)) vx2))

     (and (>= t1 0) (>= t2 0)
          (let ([x (+ x1 (* t1 vx1))]) (<= low x high))
          (let ([y (+ y1 (* t1 vy1))]) (<= low y high)))]))

;; intersections-in : (Listof Hailstone) * Int * Int -> Int
(define (intersections-in hs low high)
  (for/count ([h1*h2 (in-combinations hs 2)])
    (match-define (list h1 h2) h1*h2)
    (cross-in? h1 h2 low high)))

(module+ test
  (check-equal? (intersections-in hs 7 27) 2))

(module+ part1
  (define hs (call-with-input-file "input" parse-input))
  (intersections-in hs 200000000000000 400000000000000))
