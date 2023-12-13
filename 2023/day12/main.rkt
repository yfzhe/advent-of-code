#lang racket
(require threading
         "memo.rkt")

(module+ test
  (require rackunit))

;; parse-line : String -> String * (Listof Number)
(define (parse-line line)
  (match-define (list records sizes) (string-split line))
  (values records
          (~> (string-split sizes ",")
              (map string->number _))))

;; count-arrangements : String * (Listof Number) -> Int
(define (count-arrangements records numbers)
  (count* (string->list records) numbers))

;; count* : (Listof Char) * (Listof Number) -> Int
(define/memo (count* chars nums)
  (match* (chars nums)
    [('() '()) 1]
    [('() _) 0]
    [(_ _)
     #:when (< (length chars) (+ (apply + nums) (length nums) -1))
     0]
    [((cons #\. cs) _) (count* cs nums)]
    [((cons #\# cs) '()) 0]
    [((cons #\# cs) (cons m ns))
     (count*/damaged cs (sub1 m) ns)]
    [((cons #\? cs) _)
     (+ (count* (cons #\. cs) nums)
        (count* (cons #\# cs) nums))]))

;; count*/damaged:
;;   handle the situation when the first element of `chars` is #.
(define (count*/damaged chars m ns)
  (match* (chars m)
    [('() 0) (count* chars ns)]
    [('() _) 0]
    [((cons #\# cs) 0) 0]
    [((cons _   cs) 0) (count* cs ns)]
    [((cons #\. cs) _) 0]
    [((cons _   cs) _) (count*/damaged cs (sub1 m) ns)]))

(module+ test
  (check-equal? (count-arrangements "???.###" '(1 1 3)) 1)
  (check-equal? (count-arrangements ".??..??...?##." '(1 1 3)) 4)
  (check-equal? (count-arrangements "?###????????" '(3 2 1)) 10))

(module+ part1
  (call-with-input-file "input"
    (lambda (in)
      (for/sum ([line (in-lines in)])
        (call-with-values (lambda () (parse-line line)) count-arrangements)))))

(module+ part2
  (call-with-input-file "input"
    (lambda (in)
      (for/sum ([line (in-lines in)])
        (define-values (records numbers) (parse-line line))
        (count-arrangements (string-join (make-list 5 records) "?")
                            (apply append (make-list 5 numbers)))))))
