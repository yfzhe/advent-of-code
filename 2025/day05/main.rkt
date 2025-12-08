#lang racket
(require data/integer-set
         (prefix-in list: racket/list))

(module+ test
  (require rackunit))

;; parse-input : Input-Port -> (Values (Listof Range) (Listof Int))
(define (parse-input in)
  (define-values (rngs empty+ids)
    (list:splitf-at (port->lines in)
                    (lambda (l) (not (equal? l "")))))
  (values (map parse-range rngs)
          (map string->number (cdr empty+ids))))

;; parse-range: String -> (List Int Int)
(define (parse-range line)
  (map string->number
       (string-split line "-")))

;; build-integer-set : (Listof Range) -> Integer-Set
(define (build-integer-set ranges)
  (for/fold ([acc (make-range)])
            ([rng (in-list ranges)])
    (union acc (apply make-range rng))))

;; count-ids-in : Integer-Set * (Listof Int) -> Natural
(define (count-ids-in iset ids)
  (list:count (lambda (id) (member? id iset)) ids))

(module+ test
  (define-values (ranges ids)
    (call-with-input-file "test" parse-input))
  (define iset (build-integer-set ranges))

  (check-equal? (count-ids-in iset ids) 3))

(module+ main
  (define-values (ranges ids)
    (call-with-input-file "input" parse-input))
  (define iset (build-integer-set ranges))

  (count-ids-in ranges ids)
  (count iset))
