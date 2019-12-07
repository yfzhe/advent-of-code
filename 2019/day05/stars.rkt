#lang racket

(module+ test
  (require rackunit))

;; Program: (Vectorof Integer)

(define (parse-program in)
  (list->vector
   (map string->number
        (string-split (read-line in) ","))))

;; runner: a intcode program runner
;; it contains a program, and a pointer
(struct runner (prog [pos #:mutable]))

(define (make-runner prog)
  (runner (vector-copy prog) 0))

(define (runner-pos-move! runner offset)
  (set-runner-pos! runner
                   (+ (runner-pos runner) offset)))

(define runner-pos-jump! set-runner-pos!)

(define (parse-modes+opcode value)
  (quotient/remainder value 100))

(define (get-modes modes idx)
  (modulo (quotient modes (expt 10 idx))
          10))

(module+ test
  (check-equal? (get-modes 10 0) 0)
  (check-equal? (get-modes 11 0) 1)
  (check-equal? (get-modes 10 1) 1)
  (check-equal? (get-modes 10 2) 0))

;; prog-read: Program * Mode * Index -> Integer
(define (prog-read prog mode input-idx)
  (case mode
    [(0) ; position mode
     (vector-ref prog (vector-ref prog input-idx))]
    [(1) ; immedrate mode
     (vector-ref prog input-idx)]))

;; prog-write!: Program * Mode * Index * Integer -> Void
(define (prog-write! prog mode idx val)
  (case mode
    [(0) ; position mode
     (vector-set! prog (vector-ref prog idx) val)]))

(define (prog-read* prog modes base-pos offset)
  (prog-read prog
             (get-modes modes (sub1 offset))
             (+ base-pos offset)))

;; run-program: Program * Inputs -> Outputs
;; Inputs: a list of input (Listof Number)
;; Outputs: a reversed list of output (Listof Number)
(define (runner-run runner input)
  (let loop ([input input] [output '()])
    (define prog (runner-prog runner))
    (define pos (runner-pos runner))

    (define-values (modes opcode)
      (parse-modes+opcode (vector-ref prog pos)))
    (case opcode
      [(99) output]
      [(1 2) ; plus / multiply
       (define operand0 (prog-read* prog modes pos 1))
       (define operand1 (prog-read* prog modes pos 2))

       (define op (case opcode [(1) +] [(2) *]))
       (define result (op operand0 operand1))

       (prog-write! prog (get-modes modes 2) (+ pos 3) result)
       (runner-pos-move! runner 4)
       (loop input output)]
      [(3) ; read from input
       (prog-write! prog 0 (+ pos 1) (car input))
       (runner-pos-move! runner 2)
       (loop (cdr input) output)]
      [(4) ; write to output
       (define new-out (prog-read prog 0 (+ pos 1)))
       (runner-pos-move! runner 2)
       (loop input (cons new-out output))]
      [(5 6) ; jump-if-true / jump-if-false
       (define operand0 (prog-read* prog modes pos 1))
       (define operand1 (prog-read* prog modes pos 2))

       (define op
         (case opcode [(5) (compose not zero?)] [(6) zero?]))
       
       (if (op operand0)
           (runner-pos-jump! runner operand1)
           (runner-pos-move! runner 3))
       (loop input output)]
      [(7 8) ; less than / equals
       (define operand0 (prog-read* prog modes pos 1))
       (define operand1 (prog-read* prog modes pos 2))

       (define op (case opcode [(7) <] [(8) =]))
       (define result (if (op operand0 operand1) 1 0))

       (prog-write! prog 0 (+ pos 3) result)
       (runner-pos-move! runner 4)
       (loop input output)])))

(module+ star1
  (call-with-input-file "input.txt"
    (lambda (in)
      (car (runner-run (make-runner (parse-program in)) '(1))))))

(module+ star2
  (call-with-input-file "input.txt"
    (lambda (in)
      (car (runner-run (make-runner (parse-program in)) '(5))))))

(provide parse-program
         make-runner
         runner-run)

