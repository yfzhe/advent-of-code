#lang racket/base
(require racket/string racket/match)

(provide parse-program
         make-runner
         run-once run-until-halt)

(module+ test
  (require rackunit))

;; Program: (Listof Integer)

;; parse-program : (U Input-Port String) -> Program
(define (parse-program in)
  (define str
    (cond
      [(string? in) in]
      [(input-port? in) (read-line in)]))
  (map string->number (string-split str ",")))

;; runner: an intcode program executor, it contains:
;; memory (infinity length vector, use (Hash Index Integer) to represent it)
;; opcode pointer (pc)
;; relative base (bx)
(struct runner (mem pc bx) #:mutable)

(define (make-runner prog)
  (define init-memory
    (for/hash ([i (in-naturals)]
               [val (in-list prog)])
      (values i val)))
  (runner (hash-copy init-memory) 0 0))

(define (runner-pc-add! runner offset)
  (set-runner-pc! runner
                  (+ (runner-pc runner) offset)))

(define runner-pc-jump! set-runner-pc!)

(define (runner-bx-add! runner offset)
  (set-runner-bx! runner
                  (+ (runner-bx runner) offset)))

(define (parse-modes+opcode value)
  (quotient/remainder value 100))

(define (get-modes modes idx)
  (modulo (quotient modes (expt 10 idx)) 10))

;; runner-read: Runner * Mode * Index -> Integer
(define (runner-read runner mode input-idx)
  (define mem (runner-mem runner))
  (case mode
    [(0) ; position mode
     (hash-ref mem (hash-ref mem input-idx) 0)]
    [(1) ; immediate mode
     (hash-ref mem input-idx)]
    [(2) ; relative mode
     (hash-ref mem (+ (hash-ref mem input-idx)
                      (runner-bx runner))
               0)]))

;; runner-write!: Runner * Mode * Index * Integer -> Void
(define (runner-write! runner mode idx val)
  (define mem (runner-mem runner))
  (define address
    (case mode
      [(0) ; position mode
       (hash-ref mem idx)]
      [(2) ; relative mode
       (+ (hash-ref mem idx) (runner-bx runner))]))
  (hash-set! mem address val))

(define (runner-read* runner modes base-pos offset)
  (runner-read runner
               (get-modes modes (sub1 offset))
               (+ base-pos offset)))

(define (runner-write*! runner modes base offset val)
  (runner-write! runner
                 (get-modes modes (sub1 offset))
                 (+ base offset)
                 val))

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
  (define prog (runner-mem runner))
  (define pc (runner-pc runner))

  (define-values (modes opcode)
    (parse-modes+opcode (hash-ref prog pc)))

  (case opcode
    [(99) '(halt)]
    [(1 2) ; plus / multiply
     (define operand0 (runner-read* runner modes pc 1))
     (define operand1 (runner-read* runner modes pc 2))

     (define op (case opcode [(1) +] [(2) *]))
     (define result (op operand0 operand1))

     (runner-write*! runner modes pc 3 result)
     (runner-pc-add! runner 4)
     '(next)]
    [(3) ; read from input
     (runner-write*! runner modes pc 1 (car inputs))
     (runner-pc-add! runner 2)
     `(read ,(cdr inputs))]
    [(4) ; write to output
     (define out (runner-read* runner modes pc 1))
     (runner-pc-add! runner 2)
     `(write ,out)]
    [(5 6) ; jump-if-true / jump-if-false
     (define operand0 (runner-read* runner modes pc 1))
     (define operand1 (runner-read* runner modes pc 2))

     (define op
       (case opcode [(5) (compose not zero?)] [(6) zero?]))

     (if (op operand0)
         (runner-pc-jump! runner operand1)
         (runner-pc-add! runner 3))
     '(next)]
    [(7 8) ; less than / equals
     (define operand0 (runner-read* runner modes pc 1))
     (define operand1 (runner-read* runner modes pc 2))

     (define op (case opcode [(7) <] [(8) =]))
     (define result (if (op operand0 operand1) 1 0))

     (runner-write*! runner modes pc 3 result)
     (runner-pc-add! runner 4)
     '(next)]
    [(9) ; adjust the relative base
     (runner-bx-add! runner (runner-read* runner modes pc 1))
     (runner-pc-add! runner 2)
     '(next)]))

;; run-until-halt: Runner * Inputs -> Outputs
;; run until the program halts
(define (run-until-halt runner inputs)
  (let loop ([inputs inputs] [outputs '()])
    (match (run-once runner inputs)
      ['(halt) (reverse outputs)]
      ['(next) (loop inputs outputs)]
      [`(read ,remain) (loop remain outputs)]
      [`(write ,out) (loop inputs (cons out outputs))])))

(module+ test
  ; missing tests here
  )
