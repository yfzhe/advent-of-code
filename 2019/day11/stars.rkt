#lang racket

(require "../../intcode/interp.rkt")

;; Pos: Complex
;; note: y-axis is opposite

;; Board: (Mutable-Hash Pos (U 0 1))

;; Dir: Complex
;; -1 -> left, 1 -> right, i -> up, -i -> down
;; turn to left: dir * i, turn to right: dir * -i

(define (turn-dir dir arg)
  (case arg
    [(0) (* dir +i)]
    [(1) (* dir -i)]))

;; get-next-outputs: Runner * Pos * Board -> (List Integer Integer)
(define (get-next-outputs runner pos board)
  (define cur-color (hash-ref board pos 0))
  (define cur-inputs (list cur-color))
  (let loop ([outputs '()])
    (cond
      [(= (length outputs) 2) (reverse outputs)]
      [else
       (match (run-once runner cur-inputs)
         ['(halt) 'halt]
         ['(next) (loop outputs)]
         [`(read ,_) (loop outputs)]
         [`(write ,out) (loop (cons out outputs))])])))

(define (paint prog board)
  (define runner (make-runner prog))

  (let loop ([pos 0] [dir +i]
                     [count 0])
    (define outputs (get-next-outputs runner pos board))
    (match outputs
      ['halt board]
      [`(,color ,turn)
       (define new-dir (turn-dir dir turn))
       (define new-pos (+ pos new-dir))
       (hash-set! board pos color)
       (loop new-pos new-dir (add1 count))])))

(module+ star1
  (require threading)

  (call-with-input-file "input.txt"
    (lambda (in)
      (~> (parse-program in)
          (paint _ (make-hash))
          hash-keys
          length))))
