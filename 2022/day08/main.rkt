#lang racket
(require threading
         aoc-util)

(module+ test
  (require rackunit))

(define (parse-input in)
  (for/list ([line (in-lines in)])
    (for/list ([char (in-string line)])
      (- (char->integer char) 48))))

(module+ test
  (define input
    (call-with-input-file "test" parse-input)))

;; matrix ::= (listof (listof number?))

(define (matrix-width mat) (length (car mat)))
(define (matrix-height mat) (length mat))

(define (matrix-ref mat x y)
  (list-ref (list-ref mat y) x))
(define (matrix-row mat y)
  (list-ref mat y))
(define (matrix-col mat x)
  (for/list ([y (in-range (matrix-height mat))])
    (list-ref (matrix-row mat y) x)))

(define (higher-than? here points)
  (for/and ([point (in-list points)])
    (< point here)))

(define (visible? mat x y)
  (define here (matrix-ref mat x y))
  (define row (matrix-row mat y))
  (define col (matrix-col mat x))
  (or (higher-than? here (take row x))
      (higher-than? here (drop row (add1 x)))
      (higher-than? here (take col y))
      (higher-than? here (drop col (add1 y)))))

(define (visibles matrix)
  (for*/count ([x (in-range (matrix-width matrix))]
               [y (in-range (matrix-height matrix))])
    (visible? matrix x y)))

(module+ test
  (check-equal? (visibles input) 21))

(module+ part1
 (call-with-input-file "input"
   (lambda~> parse-input visibles)))

(define (viewing-dist here points)
  (for/count ([point (in-list points)] #:final (>= point here)) #t))

(define (scenic-score mat x y)
  (define here (matrix-ref mat x y))
  (define row (matrix-row mat y))
  (define col (matrix-col mat x))
  (* (viewing-dist here (reverse (take row x)))
     (viewing-dist here (drop row (add1 x)))
     (viewing-dist here (reverse (take col y)))
     (viewing-dist here (drop col (add1 y)))))

(module+ test
  (check-equal? (scenic-score input 2 1) 4)
  (check-equal? (scenic-score input 2 3) 8))

(define (highest mat)
  (for*/fold ([acc 0])
             ([x (in-range (matrix-width mat))]
              [y (in-range (matrix-height mat))])
    (max acc (scenic-score mat x y))))

(module+ part2
 (call-with-input-file "input"
   (lambda~> parse-input highest)))
