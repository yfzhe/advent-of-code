#lang racket
(require threading)

(module+ test
  (require rackunit))

;; Pattern = (Listof (Listof Char))
;; Mirror = (U `(row . Int) `(col . Int))

;; parse-pattern : Input-Port -> Pattern
(define (parse-pattern in)
  (~> (port->lines in)
      (map string->list _)))

(module+ test
  (define pattern1 (call-with-input-file "pattern1" parse-pattern))
  (define pattern2 (call-with-input-file "pattern2" parse-pattern)))

;; find-mirror* : Int * Pattern -> (U Mirror #f)
;;   find a mirror in one direction, from top to bottom.
;;   the arugment `n` is the target number of all the
;;     different chars between the two sides of a mirror.
(define (find-mirror* n pattern)
  (findf (lambda (i) (mirror? n pattern i))
         (range 0 (- (length pattern) 1))))

;; mirror? : Int * Pattern * Int -> Boolean
(define (mirror? n pattern i)
  (define len (length pattern))
  (let loop ([acc 0] [j i] [k (+ i 1)] #| j <- i..=0, k <- i+1..len |# )
    (cond
      [(> acc n) #f]
      [(or (< j 0) (>= k len)) (= acc n)]
      [else
       (define d (count (lambda (a b) (not (eq? a b)))
                        (list-ref pattern j) (list-ref pattern k)))
       (loop (+ acc d) (sub1 j) (add1 k))])))

;; find-mirror : Int * Pattern -> (U Mirror #f)
(define (find-mirror n pattern)
  (cond
    [(find-mirror* n pattern)
     => (lambda (row) `(row . ,(add1 row)))]
    [(find-mirror* n (transpose pattern))
     => (lambda (col) `(col . ,(add1 col)))]
    [else #f]))

(define (transpose pattern)
  (apply map list pattern))

(module+ test
  (check-equal? (find-mirror 0 pattern1) `(col . 5))
  (check-equal? (find-mirror 0 pattern2) `(row . 4))
  (check-equal? (find-mirror 1 pattern1) `(row . 3))
  (check-equal? (find-mirror 1 pattern2) `(row . 1)))

;; solve : Int -> Input-Port -> Int
(define ((solve n) in)
  (define ps (string-split (port->string in) "\n\n"))
  (for/sum ([p (in-list ps)])
    (define pattern (parse-pattern (open-input-string p)))
    (define mirror (find-mirror n pattern))
    (match mirror
      [`(col . ,col) col]
      [`(row . ,row) (* 100 row)])))

(module+ part1
  (call-with-input-file "input" (solve 0)))

(module+ part2
  (call-with-input-file "input" (solve 1)))
