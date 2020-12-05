#lang racket

(module+ test
  (require rackunit))

;;; SEAT-STR ::= [FB]{8}[LR]{3}

;;; make-binary-converter : Char * Char -> (String -> Nat)
(define (make-binary-converter zero one)
  (lambda (str)
    (for/fold ([acc 0])
              ([char (in-string str)])
      (+ (* acc 2)
         (match char [(== zero) 0] [(== one) 1])))))

(define row-pos
  (make-binary-converter #\F #\B))

(define col-pos
  (make-binary-converter #\L #\R))

;;; seat-id : String -> Nat
(define (seat-id seat-str)
  (define row (substring seat-str 0 7))
  (define col (substring seat-str 7 10))
  (+ (* (row-pos row) 8)
     (col-pos col)))

(module+ test
  (check-equal? (seat-id "BFFFBBFRRR") 567)
  (check-equal? (seat-id "FFFBBBFRRR") 119)
  (check-equal? (seat-id "BBFFBBFRLL") 820))

(module+ stars
  (define seats
    (call-with-input-file "input.txt"
      (lambda (in)
        (for/list ([line (in-lines in)])
          (seat-id line)))))

  ;; star 1: the largest seat-id
  (define total (apply max seats))
  (displayln total)

  ;; star 2: find the missing seat-id
  (for/list ([num (in-range total)]
             #:when (member (add1 num) seats)
             #:when (member (sub1 num) seats)
             #:unless (member num seats))
    num))
