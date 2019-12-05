#lang racket

(module+ test
  (require rackunit))

(define (parse-opcode+modes value)
  (quotient/remainder value 100))

(define (get-modes modes idx)
  (modulo (quotient modes (expt 10 idx))
          10))

(module+ test
  (check-equal? (get-modes 10 0) 0)
  (check-equal? (get-modes 11 0) 1)
  (check-equal? (get-modes 10 1) 1)
  (check-equal? (get-modes 10 2) 0))

;; Program: (listof Integer)

;; prog-read: Mode * Index * Program -> Integer
(define (prog-read mode input-idx prog)
  (case mode
    [(0) ; position mode
     (list-ref prog (list-ref prog input-idx))]
    [(1) ; immedrate mode
     (list-ref prog input-idx)]))

;; write-result: Mode * Index * Integer * Program -> Prog
(define (prog-write mode idx val prog)
  (case mode
    [(0) ; position mode
     (list-set prog (list-ref prog idx) val)]))

;; run-program: Program -> Input -> final output
(define (run-program prog input)
  (let loop ([prog prog] [pos 0] [input input] [output '()])
    (define-values (opcode modes)
      (parse-opcode+modes (list-ref prog pos)))
    (case opcode
      [(99) (car output)]
      [(1 2)
       (define operand0 (prog-read (get-modes modes 0) (+ pos 1) prog))
       (define operand1 (prog-read (get-modes modes 1) (+ pos 2) prog))

       (define op (if (= opcode 1) + *))
       (define result (op operand0 operand1))

       (loop (prog-write (get-modes modes 2) (+ pos 3) result prog)
             (+ pos 4)
             input
             output)]
      [(3)
       (define result-pos (list-ref prog (+ pos 1)))
       (loop (prog-write 0 (+ pos 1) input prog)
             (+ pos 2)
             #f
             output)]
      [(4)
       (loop prog
             (+ pos 2)
             input
             (cons (prog-read 0 (+ pos 1) prog)
                   output))]
      [(5 6)
       (define operand0 (prog-read (get-modes modes 0) (+ pos 1) prog))
       (define operand1 (prog-read (get-modes modes 1) (+ pos 2) prog))

       (if (if (= opcode 6) (zero? operand0) (not (zero? operand0)))
           (loop prog operand1 input output)
           (loop prog (+ pos 3) input output))]
      [(7 8)
       (define operand0 (prog-read (get-modes modes 0) (+ pos 1) prog))
       (define operand1 (prog-read (get-modes modes 1) (+ pos 2) prog))

       (define op
         (if (= opcode 7) < =))

       (define result
         (if (op operand0 operand1) 1 0))

       (loop (prog-write 0 (+ pos 3) result prog)
             (+ pos 4)
             input
             output)])))

(define prog
  (call-with-input-file "input.txt"
    (lambda (in)
      (map string->number
           (string-split (read-line in) ",")))))

(module+ star1
  (run-program prog 1))

(module+ star2
  (run-program prog 5))

