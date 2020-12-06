#lang racket
(require threading)

(module+ test
  (require rackunit))

;;; record-answers : (Listof String) -> (Hash Char Nat)
(define (record-answers answers)
  (for*/fold ([record (hash)])
             ([answer (in-list answers)]
              [char (in-string answer)])
    (hash-update record char add1 0)))

;;; count-group : (Listof String) -> Nat
(define (count-group answers)
  (~> (record-answers answers)
      hash-count))

(module+ test
  (check-equal? (count-group '("abc" "acd")) 4)
  (check-equal? (count-group '("a" "a" "b")) 2))

;;; count-group/2 : (Listof String) -> Nat
(define (count-group/2 answers)
  (~> (record-answers answers)
      hash-values
      (count (curry = (length answers)) _)))

(module+ test
  (check-equal? (count-group/2 '("abc" "acd")) 2)
  (check-equal? (count-group/2 '("a" "a" "b")) 0))

;;; main : Input-Port -> ((Listof String) -> Nat) -> Nat
(define (main in counter)
  (~> (port->string in)
      (string-split _ "\n\n")
      (map (compose counter string-split) _)
      (apply + _)))

(module+ star1
  (call-with-input-file "input.txt"
    (lambda~> (main _ count-group))))

(module+ star2
  (call-with-input-file "input.txt"
    (lambda~> (main _ count-group/2))))
