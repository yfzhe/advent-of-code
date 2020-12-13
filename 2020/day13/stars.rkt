#lang racket

(module+ test
  (require rackunit))

;;; parse-input : Input-Port -> (Values Int (Listof (U Int 'x)))
(define (parse-input in)
  (parameterize ([current-input-port in])
    (values
     (string->number (read-line))
     (for/list ([part (in-list (string-split (read-line) ","))])
       (if (equal? part "x") 'x (string->number part))))))

(module+ test
  (define input #<<EOF
939
7,13,x,x,59,x,31,19
EOF
    )

  (define in/thunk
    (lambda () (parse-input (open-input-string input)))))

;;; ---------------------- star 1 --------------------------

;;; find-answer : Int * (Listof (U Int 'x)) -> Int
(define (find-answer base buses)
  (apply *
         (argmin second
                 (for/list ([bus (in-list buses)]
                            #:unless (eq? bus 'x))
                   (list bus (- (modulo base (- bus))))))))

(module+ test
  (check-equal? (call-with-values in/thunk find-answer) 295))

(module+ star1
  (call-with-input-file "input.txt"
    (lambda (in)
      (call-with-values (lambda () (parse-input in))
                        find-answer))))

;;; ---------------------- star 2 --------------------------
(require math/number-theory) ;; use chinese remainder theorem

;;; build-equations : (Listof (U Int 'x)) -> (Values (Listof Int) (Listof Int))
;;;   input:  a list of bus-id (or 'x);
;;;   output: a list of minutes after that timestemp (REMAINDERS) and,
;;;           a list of bus-id (DIVISORS).
(define (build-equations buses)
  (for/lists (idx bus-id)
             ([bus (in-list buses)]
              [i (in-naturals)]
              #:unless (eq? bus 'x))
    (values (- i) bus)))

;;; find-answer/2 : Input-Port -> Int
;;;   group all things together
(define (find-answer/2 in)
  (call-with-values (lambda () (parse-input in))
                    (lambda (_ buses)
                      (call-with-values (lambda () (build-equations buses))
                                        solve-chinese))))

(module+ test
  (define find-answer/2* (compose find-answer/2 open-input-string))
  (check-equal? (find-answer/2* input) 1068781)
  (check-equal? (find-answer/2* "\n17,x,13,19") 3417)
  (check-equal? (find-answer/2* "\n67,7,59,61") 754018)
  (check-equal? (find-answer/2* "\n67,x,7,59,61") 779210)
  (check-equal? (find-answer/2* "\n67,7,x,59,61") 1261476)
  (check-equal? (find-answer/2* "\n1789,37,47,1889") 1202161486))

(module+ star2
  (call-with-input-file "input.txt" find-answer/2))
