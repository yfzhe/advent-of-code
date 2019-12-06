#lang racket

(module+ test
  (require rackunit))

;; Object: String

;; Map: (Hash Object (Listof Object))
(define (parse-map input)
  (for/fold ([acc (hash)])
            ([line (in-lines input)])
    (match-define (list base around)
      (string-split line ")"))
    (hash-update acc base (λ (children) (cons around children)) null)))

;; calc-depths: Map -> (Hash Object Number)
(define (calc-depth data)
  (define acc (make-hash))
  (let loop ([cur "COM"] [cur-depth 0])
    (define new-acc (hash-set! acc cur cur-depth))
    (when (hash-ref data cur #f)
      (for-each (lambda (obj)
                  (loop obj (add1 cur-depth)))
                (hash-ref data cur))))
  acc)

(define (main input)
  (define map (parse-map input))
  (define depths (calc-depth map))
  (for/sum ([depth (in-hash-values depths)]) depth))

(module+ test
  (define input
    "COM)B
B)C
C)D
D)E
E)F
B)G
G)H
D)I
E)J
J)K
K)L")

  (main (open-input-string input)))

(module+ star1
  (call-with-input-file "input.txt"
    (lambda (in)
      (main in))))
