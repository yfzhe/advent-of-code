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

;; Inputs: a list of input (Listof Number)
;; Outputs: a reversed list of output (Listof Number)

;; Runner-Return: poor man's datatype
;; - (halt): meet the opcode 99, the program halts
;; - (next): no io, just goto next instruction
;; - (read (Listof Integer)): consume a input, return the remain
;; - (write Integer): print a output

;; run-once: Runner * Inputs -> Runner-Return
;; run the current instruction
(define (run-once runner inputs)
  (define prog (runner-prog runner))
  (define pos (runner-pos runner))
  (define-values (modes opcode)
    (parse-modes+opcode (vector-ref prog pos)))

  (case opcode
    [(99) '(halt)]
    [(1 2) ; plus / multiply
     (define operand0 (prog-read* prog modes pos 1))
     (define operand1 (prog-read* prog modes pos 2))

     (define op (case opcode [(1) +] [(2) *]))
     (define result (op operand0 operand1))

     (prog-write! prog (get-modes modes 2) (+ pos 3) result)
     (runner-pos-move! runner 4)
     '(next)]
    [(3) ; read from input
     (prog-write! prog 0 (+ pos 1) (car inputs))
     (runner-pos-move! runner 2)
     `(read ,(cdr inputs))]
    [(4) ; write to output
     (define out (prog-read prog 0 (+ pos 1)))
     (runner-pos-move! runner 2)
     `(write ,out)]
    [(5 6) ; jump-if-true / jump-if-false
     (define operand0 (prog-read* prog modes pos 1))
     (define operand1 (prog-read* prog modes pos 2))

     (define op
       (case opcode [(5) (compose not zero?)] [(6) zero?]))

     (if (op operand0)
         (runner-pos-jump! runner operand1)
         (runner-pos-move! runner 3))
     '(next)]
    [(7 8) ; less than / equals
     (define operand0 (prog-read* prog modes pos 1))
     (define operand1 (prog-read* prog modes pos 2))

     (define op (case opcode [(7) <] [(8) =]))
     (define result (if (op operand0 operand1) 1 0))

     (prog-write! prog 0 (+ pos 3) result)
     (runner-pos-move! runner 4)
     '(next)]))

(define (run-until-halt runner inputs)
  (let loop ([inputs inputs] [outputs '()])
    (match (run-once runner inputs)
      ['(halt) outputs]
      ['(next) (loop inputs outputs)]
      [`(read ,remain) (loop remain outputs)]
      [`(write ,out) (loop inputs (cons out outputs))])))

(module+ star1
  (call-with-input-file "input.txt"
    (lambda (in)
      (car (run-until-halt (make-runner (parse-program in)) '(1))))))

(module+ star2
  (call-with-input-file "input.txt"
    (lambda (in)
      (car (run-until-halt (make-runner (parse-program in)) '(5))))))

(provide parse-program
         make-runner
         run-once run-until-halt)

