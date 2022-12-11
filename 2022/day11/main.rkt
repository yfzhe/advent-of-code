#lang racket
(require threading
         megaparsack megaparsack/text
         data/monad data/applicative
         racket/syntax
         data/queue)

(module+ test
  (require rackunit))

;; ---------- DATA STRUCTURE ----------

(struct monkey
  (items operation test if-true if-false [inspect-count #:mutable])
  #:transparent)

(define (make-monkey items operation test if-true if-false)
  (monkey (list->queue items)
          operation
          test if-true if-false
          0))

(define (list->queue lst)
  (let ([q (make-queue)])
    (for ([e (in-list lst)]) (enqueue! q e))
    q))

;; ---------- PARSE ----------

(define (parse-input in)
  (~>
      (parse-string monkeys/p (port->string in))
      parse-result!))

(define operator/p
  (do [char <- any-char/p]
      (pure (format-symbol "~a" char))))
(define operand/p
  (or/p (do (string/p "old") (pure 'old))
        integer/p))
(define operation/p
  (do (string/p "new = ")
      [lhs <- operand/p]
      space/p
      [op <- operator/p]
      space/p
      [rhs <- operand/p]
      (pure (list op lhs rhs))))

(define monkey/p
  (do (string/p "Monkey ")
      integer/p
      (string/p ":\n  Starting items: ")
      [items <- (many/p integer/p #:sep (string/p ", "))]
      (string/p "\n  Operation: ")
      [operation <- operation/p]
      (string/p "\n  Test: divisible by ")
      [test <- integer/p]
      (string/p "\n    If true: throw to monkey ")
      [if-true <- integer/p]
      (string/p "\n    If false: throw to monkey ")
      [if-false <- integer/p]
      (string/p "\n")
      (pure (make-monkey items operation test if-true if-false))))

(define monkeys/p
  (do [monkeys <- (many/p monkey/p #:sep (char/p #\newline))]
      (pure monkeys)))

;; ---------- PART 1 & 2 ----------

(define ((do-one-round relief?) monkeys)
  (define factor (apply * (map monkey-test monkeys)))

  (for ([mky (in-list monkeys)])
    (match-define (monkey items operation test if-true if-false _) mky)
    (let loop ()
      (unless (queue-empty? items)
        (let* ([item (dequeue! items)]
               [new (interp-operation operation item)]
               [new (if relief? (quotient new 3) new)]
               [new (remainder new factor)]
               [target (if (zero? (remainder new test)) if-true if-false)])
          (enqueue! (monkey-items (list-ref monkeys target)) new)
          (set-monkey-inspect-count! mky (add1 (monkey-inspect-count mky)))
          (loop)))))
  monkeys)

(define (interp-operation operation old)
  (match-define (list op lhs rhs) operation)
  (define operator (match op ['+ +] ['* *]))
  (operator (if (equal? lhs 'old) old lhs)
            (if (equal? rhs 'old) old rhs)))

(define (repeat times proc value)
  (cond
    [(zero? times) value]
    [else (repeat (sub1 times) proc (proc value))]))

(define (answer part monkeys)
  (define-values (times relief?)
    (match part
      ['part1 (values 20 #t)]
      ['part2 (values 10000 #f)]))

  (~> (repeat times (do-one-round relief?) monkeys)
      (map monkey-inspect-count _)
      (sort _ >)
      (take _ 2)
      (apply * _)))

(module+ test
  (check-equal? (call-with-input-file "test"
                  (lambda~> parse-input (answer 'part1 _)))
                10605)

  (check-equal? (call-with-input-file "test"
                  (lambda~> parse-input (answer 'part2 _)))
                2713310158))

(module+ part1
  (call-with-input-file "input"
    (lambda~> parse-input (answer 'part1 _))))

(module+ part2
  (call-with-input-file "input"
    (lambda~> parse-input (answer 'part2 _))))
