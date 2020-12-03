#lang racket

(module+ test
  (require rackunit))

;;; parse-line : string? -> (values number? number? char? string?)
(define (parse-line line)
  (match-define (list _
                      (app string->number num1)
                      (app string->number num2)
                      (app (compose first (curry string->list)) char)
                      password)
    (regexp-match #px"(\\d+)-(\\d+) ([a-z]): (.+)" line))
  (values num1 num2 char password))

;;; validator? = (number? * number? * char? * string? -> boolean?)
;;;
;;; make-valid? : validator? -> (string? -> boolean?)
(define (make-valid? valiator)
  (lambda (line)
    (call-with-values (lambda () (parse-line line))
                      valiator)))

;;; ---------------------- star 1 --------------------------
(define valid-password?
  (make-valid?
   (lambda (low high char password)
     (<= low
         (count (curry equal? char) (string->list password))
         high))))

(module+ test
  (check-true (valid-password? "1-3 a: abcde"))
  (check-false (valid-password? "1-3 b: cdefg"))
  (check-true (valid-password? "2-9 c: ccccccccc")))

(module+ star1
  (call-with-input-file "input.txt"
    (lambda (in)
      (sequence-count valid-password? (in-lines in)))))

;;; ---------------------- star 2 --------------------------
(define valid-password?/2
  (make-valid?
   (lambda (idx1 idx2 char password)
     (xor (eq? char (string-ref password (sub1 idx1)))
          (eq? char (string-ref password (sub1 idx2)))))))

(module+ test
  (check-true (valid-password?/2 "1-3 a: abcde"))
  (check-false (valid-password?/2 "1-3 b: cdefg"))
  (check-false (valid-password?/2 "2-9 c: ccccccccc")))

(module+ star2
  (call-with-input-file "input.txt"
    (lambda (in)
      (sequence-count valid-password?/2 (in-lines in)))))
