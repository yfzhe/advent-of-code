#lang racket
(require threading)

(module+ test
  (require rackunit))

;; parse-sequence : String -> (Listof Number)
(define (parse-sequence line)
  (~> (string-split line)
      (map string->number _)))

;; predicate* : (Listof Number) * ((Listof Number) * Number -> Number) -> Number
(define (predicate* lst update)
  (define diff (differences lst))
  (if (andmap (lambda (x) (= x 0)) diff)
      (car lst)
      (update lst (predicate* diff update))))

;; differences : (Listof Number) -> (Listof Number)
(define (differences lst)
  (for/list ([a (in-list lst)]
             [b (in-list (cdr lst))])
    (- b a)))

(define (predicate/1 lst)
  (predicate* lst (lambda (l d) (+ (last l) d))))

(module+ test
  (check-equal? (predicate/1 '(0 3 6 9 12 15)) 18)
  (check-equal? (predicate/1 '(1 3 6 10 15 21)) 28)
  (check-equal? (predicate/1 '(10 13 16 21 30 45)) 68))

(module+ part1
  (time
   (call-with-input-file "input"
     (lambda (in)
       (for/sum ([line (in-lines in)])
         (predicate/1 (parse-sequence line)))))))

(define (predicate/2 lst)
  (predicate* lst (lambda (l d) (- (first l) d))))

(module+ test
  (check-equal? (predicate/2 '(10 13 16 21 30 45)) 5))

(module+ part2
  (call-with-input-file "input"
    (lambda (in)
      (for/sum ([line (in-lines in)])
        (predicate/2 (parse-sequence line))))))
