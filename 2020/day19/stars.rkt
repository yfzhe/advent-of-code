#lang racket
(require megaparsack megaparsack/text
         data/either)
(require "../../util.rkt")

(module+ test
  (require rackunit))

(define (split-input in)
  (string-split (port->string in) "\n\n"))

(define (parse-rules rules-string)
  (define rules (make-hash))

  (define (build-rule word)
    (match word
      [(regexp #px"\"(.)\"" (list _ char))
       (char-in/p char)]
      [(regexp #px"(.+) \\| (.+)" (list _ lhs rhs))
       (define left-rule (build-rule lhs))
       (define right-rule (build-rule rhs))
       (lazy/p (or/p (try/p left-rule) (try/p right-rule)))]
      [_
       (lazy/p (apply list/p (map (lambda (idx) (hash-ref rules idx))
                                  (string-split word))))]))


  (for ([line (in-list (string-split rules-string "\n"))])
    (match-define (list _ idx rule)
      (regexp-match #px"(\\d+): (.+)" line))
    (hash-set! rules idx (build-rule rule)))

  rules)

(module+ test
  (match-define (list rules-string messages)
    (call-with-input-file "test.txt" split-input))
  (define rules (parse-rules rules-string))

  (for/list ([line (in-list (string-split messages))]
             #:when (success? (parse-string (list/p (hash-ref rules "0") eof/p) line)))
    line))

(define (match-messages rules messages)
  (define rule0 (list/p (hash-ref rules "0") eof/p))
  (for/count ([line (in-list (string-split messages))])
    (success? (parse-string rule0 line))))

(module+ star1
  (match-define (list rules-string messages)
    (call-with-input-file "input.txt" split-input))
  (define rules (parse-rules rules-string))

  (match-messages rules messages))
