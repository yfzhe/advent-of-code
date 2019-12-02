#lang racket/base
(require racket/list)
(require "star1.rkt") ;; reuse `run-program` and `get-input-prog`

(define (init-prog prog noun verb)
  (list-set (list-set prog 1 noun)
            2 verb))

;; run-program*:
;;   the whole workflow
(define (run-program* prog noun verb)
  (first (run-program (init-prog prog noun verb))))

(define input-prog (get-input-prog))

(for*/first ([noun (in-range 100)]
             [verb (in-range 100)]
             [result (in-value (run-program* input-prog noun verb))]
             #:when (= result 19690720))
  (+ (* 100 noun) verb))
