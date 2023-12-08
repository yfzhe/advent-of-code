#lang racket

(module+ test
  (require rackunit))

;; Instr = 'L | 'R
;; Rules = (Hash Node (cons Node Node))

;; parse-input : Input-Port -> (List Instr) * Rules
(define (parse-input in)
  (define instr-line (read-line in))
  (read-line in)
  (values (parse-instrs instr-line)
          (parse-rules in)))

(define (parse-instrs line)
  (for/list ([char (in-string line)])
    (string->symbol (string char))))

(define (parse-rules in)
  (for/hash ([line (in-lines in)])
    (match-define (list _ from left right)
      (regexp-match #rx"(.+) = \\((.+), (.+)\\)" line))
    (values from (cons left right))))

;; next : Rules * Node * Instr -> Node
(define (next rules from instr)
  (define choices (dict-ref rules from))
  (match instr
    ['L (car choices)]
    ['R (cdr choices)]))

;; steps : Rules * (List Instr) * Node * (Node -> Boolean) -> Int
(define (steps rules instrs from end?)
  (let loop ([from from] [s 0] [is instrs])
    (cond
      [(end? from) s]
      [(null? is) (loop from s instrs)]
      [else
       (loop (next rules from (car is))
             (add1 s)
             (cdr is))])))

;; solve/1 : Input-Port -> Int
(define (solve/1 in)
  (define-values (instrs rules) (parse-input in))
  (steps rules instrs "AAA" (lambda (node) (equal? node "ZZZ"))))

(module+ test
  (check-equal? (call-with-input-file "test" solve/1) 2)
  (check-equal? (call-with-input-file "test2" solve/1) 6))

(module+ part1
  (call-with-input-file "input" solve/1))

;; solve/2 : Input-Port -> Int
(define (solve/2 in)
  (define (end? node) (string-suffix? node "Z"))
  (define-values (instrs rules) (parse-input in))
  (apply lcm
         (for/list ([node (in-hash-keys rules)]
                    #:when (string-suffix? node "A"))
           (steps rules instrs node end?))))

(module+ test
  (check-equal? (call-with-input-file "test3" solve/2) 6))

(module+ part2
  (call-with-input-file "input" solve/2))
