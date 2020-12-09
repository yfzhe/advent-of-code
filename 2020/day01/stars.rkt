#lang racket/base
(require racket/match)

(module+ test
  (require rackunit))

(define (parse-input in)
  (for/list ([line (in-lines in)])
    (string->number line)))

;; ------------------------- star 1 -------------------------

;; two-sum : (Listof Num) * Num -> (U (List Num Num) #f)
(define (two-sum nums target)
  (define hash
   (for/hash ([num (in-list nums)])
     (values (- target num) num)))
  (for/or ([num (in-list nums)])
    (define res (hash-ref hash num #f))
    (and res
         (not (= num res))
         (list num res))))

(module+ test
  (check-equal? (two-sum '(1721 979 366 299 675 1456) 2020)
                '(1721 299)))

(module+ star1
  (call-with-input-file "input.txt"
    (lambda (in)
      (define input (parse-input in))
      (match-define (list a b)
        (two-sum input 2020))
      (* a b))))

;; ------------------------- star 2 -------------------------

;; three-sum : (listof number?) * number? -> (list number? number? number?)
(define (three-sum nums target)
  (define hash
    (for*/hash ([a (in-list nums)]
                [b (in-list nums)])
      (values (- target a b) (list b a))))
  (for/or ([num (in-list nums)])
    (define res (hash-ref hash num #f))
    (and res
         (cons num res))))

(module+ test
  (check-equal? (three-sum '(1721 979 366 299 675 1456) 2020)
                '(979 366 675)))

(module+ star2
  (call-with-input-file "input.txt"
    (lambda (in)
      (define input (parse-input in))
      (match-define (list a b c)
        (three-sum (sort input <) 2020))
      (* a b c))))
