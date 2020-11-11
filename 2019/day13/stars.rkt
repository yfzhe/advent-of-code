#lang racket
(require "../../intcode/interp.rkt"
         threading)

(module+ test
  (require rackunit))

(define (draw-tiles insts)
  (define board (make-hash))
  (let loop ([remain insts])
    (match remain
      [(list) board]
      [(list x y tile-id more ...)
       (hash-set! board (cons x y) tile-id)
       (loop more)])))

(module+ star1
  (call-with-input-file "input.txt"
    (lambda~> parse-program
              make-runner
              (run-until-halt _ '())
              draw-tiles
              hash-values
              (count (curry = 2) _))))

;; FIXME: part 2

(define (play runner)
  (let loop ([paddle-x 0] [ball-x 0] [score #f])
    (define input (sgn (- ball-x paddle-x)))
    (match (next-three-outputs runner (list input))
      ['halt score]
      [(list -1 0 new-score) (loop paddle-x ball-x new-score)]
      [(list x _ 3) (loop x ball-x score)]
      [(list x _ 4) (loop paddle-x x score)]
      [_ (loop ball-x paddle-x score)])))

(define (next-three-outputs runner inputs)
  (let loop ([outputs '()])
    (cond
      [(= (length outputs) 3) (reverse outputs)]
      [else
       (match (run-once runner inputs)
         ['(halt) 'halt]
         ['(next) (loop outputs)]
         [`(read ,_) (loop outputs)]
         [`(write ,out) (loop (cons out outputs))])])))

(module+ star2
  (call-with-input-file "input.txt"
    (lambda~> parse-program
              (list-set _ 0 2)
              make-runner
              play)))
