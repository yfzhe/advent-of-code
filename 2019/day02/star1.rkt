#lang racket/base
(require racket/list)

(provide run-program) ; for part 2

(module+ test
  (require rackunit))

;; Program : (Listof Integer)

(define (run-program prog)
  (let loop ([prog prog] [pos 0])
    (define opcode (list-ref prog pos))
    (cond
      [(= opcode 99) prog]
      [(memq opcode '(1 2))
       (define operand0-pos (list-ref prog (+ pos 1)))
       (define operand1-pos (list-ref prog (+ pos 2)))
       (define result-pos (list-ref prog (+ pos 3)))

       (define operand0 (list-ref prog operand0-pos))
       (define operand1 (list-ref prog operand1-pos))

       (define op (if (= opcode 1) + *))
       (define result (op operand0 operand1))

       (loop (list-set prog result-pos result)
             (+ pos 4))])))

(module+ test
  (check-equal? (run-program '(1 0 0 0 99))
                '(2 0 0 0 99))
  (check-equal? (run-program '(2 3 0 3 99))
                '(2 3 0 6 99))
  (check-equal? (run-program '(2 4 4 5 99 0))
                '(2 4 4 5 99 0))
  (check-equal? (run-program '(1 1 1 4 99 5 6 0 9))
                '(30 1 1 4 2 5 6 0 99)))

(define (init-program prog)
  (list-set (list-set prog 1 12)
            2 2))

(module+ main
  (require racket/string)

  (define input-prog
    (call-with-input-file
      "input.txt"
      (lambda (in)
        (map string->number
             (string-split (read-line in) ",")))))

  (define prog (init-program input-prog))
  (define final-prog (run-program prog))
  (first final-prog))

