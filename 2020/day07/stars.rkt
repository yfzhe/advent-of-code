#lang racket
(require threading
         "../../util.rkt")

(module+ test
  (require rackunit))

;;; Bag = String
;;;
;;; Node = Bag
;;; Edge = Node * Node * Nat
;;; Graph = (Listof Edge)

(struct edge (from to weight) #:transparent)

;;; parse-input : Input-Port -> Graph
(define (parse-input in)
  (for*/list ([line (in-lines in)]
              [(from to-s) (in-values (lambda () (parse-rule line)))]
              [to+weight (in-list to-s)])
    (edge from (car to+weight) (cdr to+weight))))

;;; parse-rule : String -> (Values Bag (Listof (Cons Bag Nat)))
(define (parse-rule str)
  (match-define (list _ target containee)
    (regexp-match #px"(.+) bags contain (.+)." str))
  (values target
          (parse-containee containee)))

;;; parse-containee : String -> (Listof (Cons Bag Nat))
(define (parse-containee str)
  (cond
    [(equal? str "no other bags") '()]
    [else
     (for/list ([part (in-list (string-split str ", "))])
       (match-define (list _ (app string->number num) color)
         (regexp-match #px"([0-9]+) (.+) bags*" part))
       (cons color num))]))

(module+ test
  (define in
    #<<EOF
light red bags contain 1 bright white bag, 2 muted yellow bags.
dark orange bags contain 3 bright white bags, 4 muted yellow bags.
bright white bags contain 1 shiny gold bag.
muted yellow bags contain 2 shiny gold bags, 9 faded blue bags.
shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.
dark olive bags contain 3 faded blue bags, 4 dotted black bags.
vibrant plum bags contain 5 faded blue bags, 6 dotted black bags.
faded blue bags contain no other bags.
dotted black bags contain no other bags.
EOF
    )
  (define rules (parse-input (open-input-string in)))
  (check-equal? (length (vertices-towards rules "shiny gold"))
                5))

;;; connect? : Graph * Node * Node -> Boolean
(define (connect? graph from to)
  (or (equal? from to)
      (for/or ([edge (in-list graph)]
               #:when (equal? (edge-from edge) from))
        (connect? graph (edge-to edge) to))))

;;; vertices-towards : Graph * Node -> (Listof Node)
(define (vertices-towards graph target)
  (set->list
   (for/set ([edge (in-list graph)]
             #:when (connect? graph (edge-from edge) target))
     (edge-from edge))))

(module+ star1
  (call-with-input-file "input.txt"
    (lambda~> parse-input
              (vertices-towards _ "shiny gold")
              length
              sub1)))

;;; total-weight : Graph * Node -> Nat
(define (total-weight graph from)
  (add1
   (for/sum ([edge (in-list graph)]
             #:when (equal? (edge-from edge) from))
     (* (edge-weight edge)
        (total-weight graph (edge-to edge))))))

(module+ test
  (check-equal? (total-weight rules "shiny gold") 33))

(module+ star2
  (call-with-input-file "input.txt"
    (lambda~> parse-input
              (total-weight _ "shiny gold")
              sub1)))
