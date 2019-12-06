#lang racket

(module+ test
  (require rackunit))

;; Object: String

;; Map: (Hash [around : Object] [base : Object])
(define (parse-map input)
  (for/hash
            ([line (in-lines input)])
    (match-define (list base around)
      (string-split line ")"))
    (values around base)))

(define (find-path-to-COM map obj)
  (let loop ([target obj] [acc '()])
    (if (equal? target "COM")
        (cons "COM" acc)
        (loop (hash-ref map target)
              (cons target acc)))))

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

  (define map (parse-map (open-input-string input)))
  ;(find-path-to-COM map "B")
  (find-path-to-COM map "C"))

(require racket/list)
(define (get-answer input)
  (define map (parse-map input))
  (define path-to-YOU (find-path-to-COM map "YOU"))
  (define path-to-SAN (find-path-to-COM map "SAN"))

  (define-values (path-Y path-S)
    (drop-common-prefix path-to-YOU path-to-SAN))
  (+ (length path-Y) (length path-S)))

(module+ star1
  (call-with-input-file "input.txt"
    (lambda (in)
      (get-answer in))))
