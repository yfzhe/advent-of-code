#lang racket

(module+ test
  (require rackunit))

;; Cat = "x" | "m" | "a" | "s"
;; Cmp = '< | '>
;; Part = (Dict Cat Number)
;; Rule = (List Cat Cmp Number String) | (List String)
;; Workflow = (cons Symbol Rule)
;;   so (Listof Workflow) can be used as a dict

;; ---------- PARSE ----------
;; parse-input : Input-Port -> (Listof Workflow) * (Listof Part)
(define (parse-input in)
  (match-define (list workflows parts)
    (string-split (port->string in) "\n\n"))
  (values (map parse-workflow (string-split workflows))
          (map parse-part (string-split parts))))

(define (parse-workflow line)
  (match (regexp-match #px"(.+)\\{(.+)\\}" line)
    [(list _ name rules)
     (cons name
           (map parse-rule (string-split rules ",")))]))

(define (parse-rule rule)
  (match (regexp-match #px"(.)(.)(\\d+):(.+)" rule)
    [(list _ cat op num next)
     (list cat (string->symbol op) (string->number num) next)]
    [_ (list rule)]))

(define (parse-part line)
  (match (regexp-match #px"\\{(.+)\\}" line)
    [(list _ nums)
     (for/list ([num (string-split nums ",")])
       (match (regexp-match #px"(.)=(\\d+)" num)
         [(list _ cat n)
          (cons cat (string->number n))]))]))

(module+ test
  (define-values (workflows parts)
    (call-with-input-file "test" parse-input)))

;; ---------- PART 1 ----------
;; run/1 : Workflows * Part * Label -> Boolean
(define (run/1 workflows part label)
  (match label
    ["A" #t]
    ["R" #f]
    [_
     (let loop ([rules (dict-ref workflows label)])
       (match rules
         [(cons (list next) _) (run/1 workflows part next)]
         [(cons (list cat op m next) _)
          #:when ((match op ['< <] ['> >]) (dict-ref part cat) m)
          (run/1 workflows part next)]
         [(cons _ more) (loop more)]))]))

;; solve/1 : Workflows * (Listof Part) -> Number
(define (solve/1 workflows parts)
  (for/sum ([part (in-list parts)]
            #:when (run/1 workflows part "in"))
    (apply + (dict-values part))))

(module+ test
  (check-equal? (solve/1 workflows parts) 19114))

(module+ part1
  (define-values (workflows parts)
    (call-with-input-file "input" parse-input))
  (solve/1 workflows parts))

;; ---------- PART 2 ----------

;; Range = (cons Int Int) = [start, end)
;; Part* = (Dict Cat Range)

;; run/2 : Workflow * Part* -> (Listof Part*)
(define (run/2 workflows part* label)
  (match label
    ["A" (list part*)]
    ["R" (list)]
    [_
     (let loop ([rules (dict-ref workflows label)]
                [part* part*])
       (match rules
         [(cons (list next) _)
          (run/2 workflows part* next)]
         [(cons (list cat op num next) more)
          (match-define (list true-part* false-part*)
            (split-part* part* cat op num))
          (append
           (if true-part* (run/2 workflows true-part* next) '())
           (if false-part* (loop more false-part*) '()))]))]))

;; split-part* : Part* * Cat * Op * Number -> (List (U Part* #f) (U Part* #f)
;;   [0, 10) < 5 -> [0, 5) [5, 10)
;;   [0, 10) > 5 -> [6, 10) [0, 6)
;;   [0, 10) < 20 -> [0, 10) #f
(define (split-part* part* cat op num)
  (cond
    [(equal? op '>)
     (reverse (split-part* part* cat '< (add1 num)))]
    [else
     (match-define (cons start end) (dict-ref part* cat))
     (cond
       [(< num start) (list #f part*)]
       [(<= end num) (list part* #f)]
       [else (list (dict-set part* cat (cons start num))
                   (dict-set part* cat (cons num end)))])]))

(module+ test
  (check-equal? (split-part* '(("x" . (0 . 10))) "x" '> 5)
                (list '(("x" . (6 . 10))) '(("x" . (0 . 6)))))
  (check-equal? (split-part* '(("m" . (0 . 10))) "m" '< 20)
                (list '(("m" . (0 . 10))) #f)))


;; init-part* : Part*
(define init-part* '(("x" . (1 . 4001)) ("m" . (1 . 4001))
                     ("a" . (1 . 4001)) ("s" . (1 . 4001))))

;; solve/2 : Workflows -> Number
(define (solve/2 workflows)
  (define result (run/2 workflows init-part* "in"))
  (for/sum ([part* (in-list result)])
    (for/product ([rng (in-dict-values part*)])
      (- (cdr rng) (car rng)))))

(module+ test
  (check-equal? (solve/2 workflows) 167409079868000))

(module+ part2
  (define-values (workflows _)
    (call-with-input-file "input" parse-input))
  (solve/2 workflows))
