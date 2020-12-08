#lang racket
(require threading)

(module+ test
  (require rackunit))

;;; Inst = 'nop | 'acc | 'jmp
;;; Program = (Listof (Cons Inst Int))

;;; parse-input :  Input-Port -> Program
(define (parse-input in)
  (for/list ([line (in-lines in)])
    (match-define (list _
                        (app string->symbol inst)
                        (app string->number num))
      (regexp-match #px"(nop|acc|jmp) ([+-][0-9]+)" line))
    (cons inst num)))

(module+ test
  (define in
    (parse-input
     (open-input-string #<<EOF
nop +0
acc +1
jmp +4
acc +3
jmp -3
acc -99
acc +1
jmp -4
acc +6
EOF
      ))))

;;; one-step : Program * Nat * Int -> (Values Nat Int)
(define (one-step prog idx acc)
  (match (list-ref prog idx)
    [`(nop . ,_)
     (values (add1 idx) acc)]
    [`(acc . ,num)
     (values (add1 idx) (+ acc num))]
    [`(jmp . ,offset)
     (values (+ idx offset) acc)]))

;;; ---------------------- star 1 --------------------------

;;; before-loop : Program -> Int
(define (before-loop prog)
  (let loop ([idx 0] [acc 0] [run-ids (set)])
    (cond
      [(set-member? run-ids idx) acc]
      [else
       (define-values (idx* acc*) (one-step prog idx acc))
       (loop idx* acc* (set-add run-ids idx))])))

(module+ test
  (check-equal? (before-loop in) 5))

(module+ star1
  (call-with-input-file "input.txt"
    (lambda~> parse-input
              before-loop)))

;;; ---------------------- star 2 --------------------------

;;; run-program : Program -> (U Int #f)
;;;   #f indicates looping
(define (run-program prog)
  (let loop ([idx 0] [acc 0] [run-ids (set)])
    (cond
      [(set-member? run-ids idx) #f]
      [(>= idx (length prog)) acc]
      [else
       (define-values (idx* acc*) (one-step prog idx acc))
       (loop idx* acc* (set-add run-ids idx))])))

;;; flip : Program * Nat -> Program
(define (flip prog idx)
  (match (list-ref prog idx)
    [`(acc . ,_) #f]
    [`(nop . ,num)
     (list-set prog idx `(jmp . ,num))]
    [`(jmp . ,num)
     (list-set prog idx `(nop . ,num))]))

;;; find-answer/2 : Program -> Nat
(define (find-answer/2 prog)
  (for/or ([i (in-range (length prog))])
    (and~> (flip prog i)
           run-program)))

(module+ test
  (check-equal? (find-answer/2 in) 8))

(module+ star2
  (call-with-input-file "input.txt"
    (lambda~> parse-input
              find-answer/2)))
