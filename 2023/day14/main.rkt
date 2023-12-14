#lang racket
(require threading)

(module+ test
  (require rackunit))

;; Platform : (Listof (Listof Char))

(define (parse-input in)
  (~> (port->lines in)
      (map string->list _)))

;; ---------- PART 1 ----------
(define (tilt-west/row row)
  (let loop ([cs row] [acc '()] [roundeds 0] [empties 0])
    (match cs
      ['()
       (~> (make-tilted roundeds empties 0)
           (cons _ acc)
           reverse
           (apply append _))]
      [(cons #\O cs*) (loop cs* acc (add1 roundeds) empties)]
      [(cons #\. cs*) (loop cs* acc roundeds (add1 empties))]
      [(cons #\# cs*)
       (loop cs* (cons (make-tilted roundeds empties 1) acc) 0 0)])))

(define (make-tilted roundeds empties cubes)
  (append (make-list roundeds #\O)
          (make-list empties #\.)
          (make-list cubes #\#)))

(define (tilt-west platform)
  (map tilt-west/row platform))

(define tilt-north
  (lambda~> transpose
            tilt-west
            transpose))

(define tilt-east
  (lambda~> (map reverse _)
            tilt-west
            (map reverse _)))

(define tilt-south
  (lambda~> reverse
            tilt-north
            reverse))

(define (transpose m)
  (apply map list m))

(module+ test
  (check-equal? (tilt-north (call-with-input-file "test" parse-input))
                (call-with-input-file "test-out" parse-input)))

(define (total-load platform)
  (for*/sum ([(row idx) (in-indexed (reverse platform))]
             [char (in-list row)]
             #:when (eq? char #\O))
    (add1 idx)))

(module+ part1
  (call-with-input-file "input"
    (lambda~> parse-input
              tilt-north
              total-load)))

;; ---------- PART 2 ----------
(define cycle
  (lambda~> tilt-north
            tilt-west
            tilt-south
            tilt-east))

(define (find-loop p)
  (let loop ([p p] [acc (list p)])
    (define p* (cycle p))
    (cond
      [(member p* acc)
       => (lambda (pre)
            (values (sub1 (length pre))
                    (add1 (- (length acc) (length pre)))
                    (reverse acc)))]
      [else (loop p* (cons p* acc))])))

(define (repeat-cycle p n)
  (define-values (pre loop acc) (find-loop p))
  (define i (+ (remainder (- n pre) loop) pre))
  (list-ref acc i))

(module+ part2
  (call-with-input-file "input"
    (lambda (in)
      (~> (parse-input in)
          (repeat-cycle _ 1000000000)
          total-load))))
