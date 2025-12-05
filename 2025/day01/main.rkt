#lang racket

(module+ test
  (require rackunit))

(define (parse-input in)
  (for/list ([line (in-lines in)])
    (parse-line line)))

;; use an integer to represent a rotation
;; positive as turning Right, negative as turning Left
;; inspired by https://github.com/mstksg/advent-of-code/wiki/Reflections-2025#day-1
(define (parse-line line)
  (define dir (string-ref line 0))
  (* (match (string-ref line 0) [#\L -1] [#\R 1])
     (string->number (substring line 1))))

;; ---------- PART 1 ----------
(define (rotate/1 base offset)
  (define result (modulo (+ base offset) 100))
  (values result (if (zero? result) 1 0)))

(define (solve rotate offsets)
  (for/fold ([base 50]
             [cnt 0]
             #:result cnt)
            ([rotation (in-list offsets)])
    (define-values (result 0s) (rotate base rotation))
    (values result (+ cnt 0s))))

(module+ test
  (define input
    (call-with-input-file "test" parse-input))

  (check-equal? (solve rotate/1 input) 3))

(module+ main
  (define input
    (call-with-input-file "input" parse-input))

  (solve rotate/1 input))

;;; ---------- PART 2 ----------

(define (rotate/2 base offset)
  (define result (+ base offset))
  (define 0s
    (if (> offset 0)
        (- (floor (/ result 100)) (floor (/ base 100)))
        (- (floor (/ (sub1 base) 100)) (floor (/ (sub1 result) 100)))))
  (values (modulo result 100) 0s))

(module+ test
  (check-equal? (solve rotate/2 input) 6)
  (check-equal? (solve rotate/2 '(1000)) 10))

(module+ main
  (solve rotate/2 input))
