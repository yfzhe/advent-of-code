#lang racket

(module+ test
  (require rackunit
           aoc-util/test))

;; <command> ::= (forward <int>)
;;             | (down <int>)
;;             | (up <int>)
;;
;; Commands = (Listof Command)

;; parse-input : Input-Port -> Commands
(define (parse-input in)
  (for/list ([line (in-lines in)])
    (match-define (list op value) (string-split line))
    (list (string->symbol op)
          (string->number value))))

(module+ test
  (define input
    (call-with-input-file "test" parse-input)))

;; -------------------- PART 1 --------------------

;; eval : Commands -> (Values Int Int)
(define (eval cmds)
  (for/fold ([x 0] [y 0])
            ([cmd (in-list cmds)])
    (match cmd
      [`(forward ,n)
       (values (+ x n) y)]
      [`(down ,n)
       (values x (+ y n))]
      [`(up ,n)
       (values x (- y n))])))

(module+ test
  (check-values-equal? (eval input)
                       (values 15 10)))

(module+ part1
  (define input
    (call-with-input-file "input" parse-input))
  (let-values ([(x y) (eval input)])
    (* x y)))

;; -------------------- PART 2 --------------------

;; eval2 : Commands -> (Values Int Int Int)
(define (eval2 commands)
  (for/fold ([x 0] [y 0] [aim 0])
            ([command (in-list commands)])
    (match command
      [`(forward ,n)
       (values (+ x n)
               (+ y (* n aim))
               aim)]
      [`(down ,n)
       (values x y (+ aim n))]
      [`(up ,n)
       (values x y (- aim n))])))

(module+ test
  (check-values-equal? (eval2 input)
                       (values 15 60 10)))

(module+ part2
  (define input
    (call-with-input-file "input" parse-input))
  (let-values ([(x y _) (eval2 input)])
    (* x y)))
