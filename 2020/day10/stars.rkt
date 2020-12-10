#lang racket
(require threading)

(module+ test
  (require rackunit))

;;; NOTE: ignore the existence of the built-in joltage (max + 3)

;;; parse-input : Input-Port -> (Listof Nat)
;;;   parse input to list of numbers, and sort it
(define (parse-input in)
  (define nums
   (for/list ([line (in-lines in)])
     (string->number line)))
  (cons 0 (sort nums <)))

;;; ---------------------- star 1 --------------------------

;;; differences : (Listof Nat) -> (Dict Nat Nat)
(define (differences nums)
  (for/fold ([d '((3 . 1))])
            ([prev (in-list nums)]
             [next (in-list (cdr nums))])
    (dict-update d (- next prev) add1 0)))

(module+ test
  (define input #<<EOF
16
10
15
5
1
11
7
19
6
12
4
EOF
    )
  (let* ([in (parse-input (open-input-string input))]
         [dist (differences in)])
    (check-equal? (dict-ref dist 1) 7)
    (check-equal? (dict-ref dist 3) 5))

  (define input2 #<<EOF
28
33
18
42
31
14
46
20
48
47
24
23
49
45
19
38
39
11
1
32
25
35
8
17
7
9
4
2
34
10
3
EOF
    )
  (let* ([in (parse-input (open-input-string input2))]
         [dist (differences in)])
    (check-equal? (dict-ref dist 1) 22)
    (check-equal? (dict-ref dist 3) 10)))

(module+ star1
  (call-with-input-file "input.txt"
    (lambda (in)
      (define dist (differences (parse-input in)))
      (* (dict-ref dist 1) (dict-ref dist 3)))))

;;; ---------------------- star 2 --------------------------

;;; ways-to-arrange : (Listof Nat) -> Nat
(define (ways-to-arrange nums)
  ;; dynamic programming in a poor way,
  ;; maybe we can use memoization for a better implementation
  (for/fold ([acc '(1)]
             #:result (car acc))
            ([idx (in-range 1 (length nums))])
    (define ways
      (for/sum ([i (in-range 1 4)])
       (define prev-idx (- idx i))
       (cond
         [(< prev-idx 0) 0]
         [(<= (- (list-ref nums idx)
                 (list-ref nums prev-idx))
              3)
          (list-ref acc (sub1 i))]
         [else 0])))
    (cons ways acc)))

(module+ test
  (let ([in1 (parse-input (open-input-string input))])
   (check-equal? (ways-to-arrange in1) 8))
  (let ([in2 (parse-input (open-input-string input2))])
    (check-equal? (ways-to-arrange in2) 19208)))

(module+ star2
  (call-with-input-file "input.txt"
    (lambda~> parse-input
              ways-to-arrange)))
