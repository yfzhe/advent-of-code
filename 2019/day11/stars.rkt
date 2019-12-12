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

  (let loop ([pos 0] [dir +i])
    (define outputs (get-next-outputs runner pos board))
    (match outputs
      ['halt board]
      [`(,color ,turn)
       (define new-dir (turn-dir dir turn))
       (define new-pos (+ pos new-dir))
       (hash-set! board pos color)
       (loop new-pos new-dir)])))

(define (print-board board)
  (define positions (hash-keys board))
  (define xs (map real-part positions))
  (define ys (map imag-part positions))
  (define leftest (apply min xs))
  (define rightest (apply max xs))
  (define highest (apply max ys))
  (define lowest (apply min ys))

  (for ([y (in-range highest (sub1 lowest) -1)])
    (for ([x (in-range leftest (add1 rightest) 1)])
      (define pos (make-rectangular x y))
      (define color (hash-ref board pos 0))
      (display (if (zero? color) #\space #\#)))
    (newline)))

(require threading)

(module+ star1
  (call-with-input-file "input.txt"
    (lambda (in)
      (~> (parse-program in)
          (paint _ (make-hash))
          hash-count))))

(module+ star2
  (call-with-input-file "input.txt"
    (lambda (in)
      (~> (parse-program in)
          (paint _ (make-hash '((0 . 1))))
          print-board))))
