#lang racket/base
(require racket/list racket/string)
(require "star1.rkt") ;; reuse `run-program` from part 1

(define (init-prog prog noun verb)
  (list-set (list-set prog 1 noun)
            2 verb))

;; run-program*:
;;   the whole workflow
(define (run-program* prog noun verb)
  (first (run-program (init-prog prog noun verb))))

(define input-prog
  (call-with-input-file
    "input.txt"
    (lambda (in)
      (map string->number
           (string-split (read-line in) ",")))))

(for*/first ([noun (in-range 100)]
             [verb (in-range 100)]
             [result (in-value (run-program* input-prog noun verb))]
             #:when (= result 19690720))
  (+ (* 100 noun) verb))
