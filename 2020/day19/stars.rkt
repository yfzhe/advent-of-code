#lang racket
(require megaparsack megaparsack/text
         data/applicative data/monad
         data/either
         threading
         "../../util.rkt")

(module+ test
  (require rackunit))

;;; split-input : Input-Port -> (Values String String)
(define (split-input in)
  (~> (port->string in)
      (string-split _ "\n\n")
      (apply values _)))

;;; Rules = (Hash Id Parser)
;;; Id = String

;;; parse-rules : String -> Rules
(define (parse-rules rules-string)
  (define rules (make-hash))

  (define (build-rule pattern)
    (match pattern
      [(regexp #px"\"(.)\"" (list _ char))
       (char-in/p char)]
      [(regexp #px"(.+) \\| (.+)" (list _ lhs rhs))
       (define left-rule (build-rule lhs))
       (define right-rule (build-rule rhs))
       (lazy/p (or/p (try/p left-rule) (try/p right-rule)))]
      [_
       (lazy/p (apply list/p (map (lambda (idx) (hash-ref rules idx))
                                  (string-split pattern))))]))

  (for ([line (in-list (string-split rules-string "\n"))])
    (match-define (list _ id pattern)
      (regexp-match #px"(\\d+): (.+)" line))
    (hash-set! rules id (build-rule pattern)))
  rules)

;;; match-messages : Rules * String -> Nat
(define (match-messages rules messages)
  (define rule0 (list/p (hash-ref rules "0") eof/p))
  (for/count ([line (in-list (string-split messages))])
    (success? (parse-string rule0 line))))

(module+ test
  (test-case "part 1 test"
    (define-values (rules-string messages)
      (call-with-input-file "test.txt" split-input))
    (define rules (parse-rules rules-string))
    (check-equal? (match-messages rules messages) 2)))

(module+ stars
  (define-values (rules-string messages)
    (call-with-input-file "input.txt" split-input))
  (define rules (parse-rules rules-string))
  (match-messages rules messages))

;;; ---------------------- STAR 2 --------------------------
;;; rule 8: 42 | 42 8, is 42+
;;; rule 11: 42 31 | 42 11 31, is 42+ 31+ (two lists should have same length)
;;; But NOTE the rule 0 is always 8 11 (in test and input)
;;; so we can treat rule 0 as 42+ 31+ (the number of 42 should be more than 31's)

(define (update-rules! rules)
  (define rule42 (hash-ref rules "42"))
  (define rule31 (hash-ref rules "31"))
  (define rule0
    (guard/p (do [42s <- (many+/p rule42)]
                 [31s <- (many+/p rule31)]
               (pure (> (length 42s) (length 31s))))
             values))
  (hash-set! rules "0" rule0))

(module+ test
  (test-case "part 2 test"
    (define-values (rules-string messages)
      (call-with-input-file "test2.txt" split-input))
    (define rules (parse-rules rules-string))
    (update-rules! rules)
    (check-equal? (match-messages rules messages) 12)))

(module+ stars
  (update-rules! rules)
  (match-messages rules messages))
