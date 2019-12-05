#lang racket

(module+ test
  (require rackunit))

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

;; Program: (listof Integer)

;; prog-read: Program * Mode * Index -> Integer
(define (prog-read prog mode input-idx)
  (case mode
    [(0) ; position mode
     (list-ref prog (list-ref prog input-idx))]
    [(1) ; immedrate mode
     (list-ref prog input-idx)]))

;; write-result: Program * Mode * Index * Integer -> Prog
(define (prog-write prog mode idx val)
  (case mode
    [(0) ; position mode
     (list-set prog (list-ref prog idx) val)]))

(define (prog-read* prog modes base-pos offset)
  (prog-read prog
             (get-modes modes (sub1 offset))
             (+ base-pos offset)))

(require racket/trace)

(define (run-program prog input)
  (let loop ([prog prog] [pos 0] [input input] [output '()])
    (define-values (modes opcode)
      (parse-modes+opcode (list-ref prog pos)))
    (case opcode
      [(99)
       (car output)]
      [(1 2) ; plus / multiply
       (define operand0 (prog-read* prog modes pos 1))
       (define operand1 (prog-read* prog modes pos 2))

       (define op (case opcode [(1) +] [(2) *]))
       (define result (op operand0 operand1))

       (loop (prog-write prog (get-modes modes 2) (+ pos 3) result)
             (+ pos 4)
             input
             output)]
      [(3) ; read from input
       (loop (prog-write prog 0 (+ pos 1) input)
             (+ pos 2)
             #f
             output)]
      [(4) ; write to output
       (loop prog
             (+ pos 2)
             input
             (cons (prog-read prog 0 (+ pos 1))
                   output))]
      [(5 6) ; jump-if-true / jump-if-false
       (define operand0 (prog-read* prog modes pos 1))
       (define operand1 (prog-read* prog modes pos 2))

       (define op
         (case opcode [(5) (compose not zero?)] [(6) zero?]))
       (define result
         (if (op operand0) operand1 (+ pos 3)))

       (loop prog result input output)]
      [(7 8) ; less than / equals
       (define operand0 (prog-read* prog modes pos 1))
       (define operand1 (prog-read* prog modes pos 2))

       (define op (case opcode [(7) <] [(8) =]))
       (define result (if (op operand0 operand1) 1 0))

       (loop (prog-write prog 0 (+ pos 3) result)
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

