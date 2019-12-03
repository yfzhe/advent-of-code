#lang racket/base
(require racket/string racket/match racket/set)

(module+ test
  (require rackunit))

;; Wire: (Listof Wire-Node)
;; Wire-Node: (Cons (U #\U #\D #\L #\R) Number)

(define (parse-wire str)
  (map parse-wire-node (string-split str ",")))

(define (parse-wire-node step)
  (define direction (string-ref step 0))
  (define length (substring step 1))
  (cons direction (string->number length)))

(module+ test
  (check-equal? (parse-wire "R8,U5,L5,D3")
                '((#\R . 8) (#\U . 5) (#\L . 5) (#\D . 3))))

;; Pos: (Cons Integer Integer)

(define *central-port* '(0 . 0))

;; make-pos-updater: (U 'x 'y) * (Integer -> Integer) -> (Pos -> Pos)
(define (make-pos-updater field updater)
  (lambda (pos)
    (case field
      [(x) (cons (updater (car pos)) (cdr pos))]
      [(y) (cons (car pos) (updater (cdr pos)))])))

;; print-wire: Wire -> (Hash Pos Integer)
;; use a hash map as a canvas, and print **each** point of the wire on it
(define (print-wire wire)
  (for/fold ([base '(0 . 0)]
             [step 0]
             [canvas (hash)]
             #:result canvas)
            ([node (in-list wire)])
    (define amount (cdr node))
    (define updater
      (match (car node)
        [#\U (make-pos-updater 'y add1)]
        [#\D (make-pos-updater 'y sub1)]
        [#\L (make-pos-updater 'x sub1)]
        [#\R (make-pos-updater 'x add1)]))
    
    (for/fold ([pos base]
               [step step]
               [canvas canvas])
              ([_ (in-range amount)])
      (let* ([next-point (updater pos)]
             [next-step (add1 step)]
             [next-canvas
              (hash-set canvas (updater pos) next-step)])
        (values next-point next-step next-canvas)))))

;; find-answer: Wire * Wire * (Pos * Steps * Steps -> Integer) -> Integer
;; get the intersection of two wires, and use a "rule" to find the answer
(define (find-answer wire1 wire2 proc)
  (define canvas1 (print-wire wire1))
  (define canvas2 (print-wire wire2))

  (for/fold ([acc +inf.0]
             #:result (inexact->exact acc))
            ([point (in-hash-keys canvas1)])
    (define steps1 (hash-ref canvas1 point))
    (define steps2 (hash-ref canvas2 point #f))

    (if (not steps2)
        acc
        (let ([cur (proc point steps1 steps2)])
          (min acc cur)))))

(define (part1-rule pos steps1 steps2)
  (+ (abs (car pos)) (abs (cdr pos))))

(define (part2-rule pos steps1 steps2)
  (+ steps1 steps2))

(module+ test
  (require racket/match)

  (define cases
    '(("R75,D30,R83,U83,L12,D49,R71,U7,L72"
       "U62,R66,U55,R34,D71,R55,D58,R83"
       159
       610)
      ("R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51"
       "U98,R91,D20,R16,D67,R40,U7,R15,U6,R7"
       135
       410)))

  (for ([a-case (in-list cases)])
    (match-define (list wire1-str wire2-str part1-ans part2-ans) a-case)
    (define wire1 (parse-wire wire1-str))
    (define wire2 (parse-wire wire2-str))

    (check-equal? (find-answer wire1 wire2 part1-rule) part1-ans)
    (check-equal? (find-answer wire1 wire2 part2-rule) part2-ans)))

(module+ star1
  (call-with-input-file "input.txt"
    (lambda (in)
      (define wire1 (parse-wire (read-line in)))
      (define wire2 (parse-wire (read-line in)))
      (find-answer wire1 wire2 part1-rule))))

(module+ star2
  (call-with-input-file "input.txt"
    (lambda (in)
      (define wire1 (parse-wire (read-line in)))
      (define wire2 (parse-wire (read-line in)))
      (find-answer wire1 wire2 part2-rule))))

