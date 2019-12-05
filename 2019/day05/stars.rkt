#lang racket

(module+ test
  (require rackunit))

(define (parse-opcode+params value)
  (define-values (params opcode)
    (quotient/remainder value 100))
  (values
   opcode
   (let loop ([num params] [acc '()])
     (cond
       [(< num 10) (reverse (cons num acc))]
       [else (let-values ([(q r) (quotient/remainder num 10)])
               (loop q (cons r acc)))]))))

;; (parse-opcode+params 1002) => (values 2 '(0 1))

(define (get-param params idx)
  (if (< idx (length params))
      (list-ref params idx)
      0))

;; Program: (listof Integer)

;; read-operand: Mode * Index * Program -> Integer
(define (read-operand mode input-idx prog)
  (case mode
    [(0) ; position mode
     (list-ref prog (list-ref prog input-idx))]
    [(1) ; immedrate mode
     (list-ref prog input-idx)]))

;; write-result: Mode * Index * Integer * Program -> Prog
(define (write-result mode idx val prog)
  (case mode
    [(0) ; position mode
     (list-set prog (list-ref prog idx) val)]))

;; run-program: Program -> (Listof output)
(define (run-program prog)
  (let loop ([prog prog] [pos 0] [port 1] [output '()])
    (define-values (opcode params)
      (parse-opcode+params (list-ref prog pos)))
    (case opcode
      [(99) output]
      [(1 2)
       (define operand0 (read-operand (get-param params 0) (+ pos 1) prog))
       (define operand1 (read-operand (get-param params 1) (+ pos 2) prog))

       (define op (if (= opcode 1) + *))
       (define result (op operand0 operand1))

       (loop (write-result (get-param params 2) (+ pos 3) result prog)
             (+ pos 4)
             port
             output)]
      [(3)
       (define result-pos (list-ref prog (+ pos 1)))
       (loop (write-result 0 (+ pos 1) port prog)
             (+ pos 2)
             #f
             output)]
      [(4)
       (loop prog
             (+ pos 2)
             port
             (cons (read-operand 0 (+ pos 1) prog)
                   output))])))

(define prog
  (call-with-input-file "input.txt"
    (lambda (in)
      (map string->number
           (string-split (read-line in) ",")))))

(module+ star1
  (car (run-program prog)))
