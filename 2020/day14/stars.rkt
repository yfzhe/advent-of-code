#lang racket
(require threading)

(module+ test
  (require rackunit))

;;; <program> ::= <clause> +
;;; <clause> ::= (mask <string>)
;;;            | (mem <address> <value>)

;;; parse-input : Input-Port -> Program
(define (parse-input in)
  (for/list ([line (in-lines in)])
    (match line
      [(regexp #px"mask = ([X01]+)" (list _ mask))
       `(mask ,mask)]
      [(regexp #px"mem\\[(\\d+)\\] = (\\d+)" (list _ address value))
       `(mem ,(string->number address)
             ,(string->number value))])))

(module+ test
  (define input #<<EOF
mask = XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X
mem[8] = 11
mem[7] = 101
mem[8] = 0
EOF
    ))

;;; ---------------------- star 1 --------------------------

;;; number->binary-string : Num -> String
(define (number->binary-string num)
  (~r num #:base 2 #:min-width 36 #:pad-string "0"))

;;; apply-mask : String * Num -> Num
(define (apply-mask mask num)
  (define chars
    (for/list ([mask-char (in-string mask)]
               [num-char (in-string (number->binary-string num))])
      (if (eq? mask-char #\X) num-char mask-char)))
  (string->number (list->string chars) 2))

(define (run program)
  (for/fold ([mask #f] [memory (hash)] #:result memory)
            ([clause (in-list program)])
    (match clause
      [`(mask ,new-mask)
       (values new-mask memory)]
      [`(mem ,address ,value)
       (values mask
               (hash-set memory address (apply-mask mask value)))])))

(module+ test
  (check-equal? (run (parse-input (open-input-string input)))
                (hash 7 101 8 64)))

(module+ star1
  (call-with-input-file "input.txt"
    (lambda~> parse-input
              run
              hash-values
              (apply + _))))

;;; ---------------------- star 2 --------------------------

;;; apply-mask/2 : String * Num -> (Listof Num)
(define (apply-mask/2 mask address)
  (define chars
    (for/list ([mask-char (in-string mask)]
               [num-char (in-string (number->binary-string address))])
      (match mask-char
        [#\0 num-char]
        [#\1 #\1]
        [#\X #\X])))
  (map (compose1 string->number list->string)
       (build-numbers chars)))

;;; build-numbers : (Listof Char) -> (Listof (Listof Char))
(define (build-numbers chars)
  (match chars
    ['() '(())]
    [(cons #\X tail)
     (append-map (lambda (lst) (list (cons #\0 lst) (cons #\1 lst)))
                 (build-numbers tail))]
    [(cons head tail)
     (map (lambda (lst) (cons head lst))
          (build-numbers tail))]))

;;; run/2 : Program -> Memory
(define (run/2 program)
  (for/fold ([mask #f] [memory (hash)] #:result memory)
            ([clause (in-list program)])
    (match clause
      [`(mask ,new-mask)
       (values new-mask memory)]
      [`(mem ,address ,value)
       (values mask
               (for/fold ([acc memory])
                         ([add (apply-mask/2 mask address)])
                 (hash-set acc add value)))])))

(module+ test
  (define input2 #<<EOF
mask = 000000000000000000000000000000X1001X
mem[42] = 100
mask = 00000000000000000000000000000000X0XX
mem[26] = 1
EOF
    )
  (check-equal? (apply + (hash-values (run/2 (parse-input (open-input-string input2)))))
                208))

(module+ star2
  (call-with-input-file "input.txt"
    (lambda~> parse-input
              run/2
              hash-values
              (apply + _))))
