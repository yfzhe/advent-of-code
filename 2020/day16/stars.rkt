#lang racket

(module+ test
  (require rackunit))

;;; ---------------------- FIELD --------------------------
(struct field (name a b c d) #:transparent)

(define (valid-in-field? r num)
  (match-define (field _ a b c d) r)
  (or (<= a num b) (<= c num d)))

;;; ------------------- PARSE INPUT -----------------------
;;; parse-input : Input-Port -> (Values (Listof Field) Ticket (Listof Ticket))
(define (parse-input in)
  (match-define (list fields my-ticket nearby)
    (string-split (port->string in) "\n\n"))
  (values (parse-fields fields)
          (car (parse-tickets my-ticket))
          (parse-tickets nearby)))

(define-match-expander num-str
  (lambda (stx)
    (syntax-case stx ()
      [(_ id)
       #'(app string->number (? number? id))])))

;;; parse-fields : String -> (Listof Field)
(define (parse-fields lines)
  (for/list ([line (in-list (string-split lines "\n"))])
    (match-define (list _ name
                        (num-str min1) (num-str max1)
                        (num-str min2) (num-str max2))
      (regexp-match #px"([a-z ]+): (\\d+)-(\\d+) or (\\d+)-(\\d+)" line))
    (field name min1 max1 min2 max2)))

;;; parse-ticket : String -> Ticket
(define (parse-ticket line)
  (map string->number (string-split line ",")))

;;; parse-tickets : String -> (Listof Ticket)
;;;   (ignore first line)
(define (parse-tickets lines)
  (map parse-ticket (cdr (string-split lines "\n"))))

(module+ test
  (define-values (fields _ nearby)
    (call-with-input-file "test.txt" parse-input)))

;;; ---------------------- star 1 ----------------------

;;; valid-for-some-field? : (Listof Field) * Num -> Boolean
(define (valid-for-some-field? fields part)
  (ormap (lambda (field) (valid-in-field? field part)) fields))

;;; tickets-error-rate : (Listof Field) * (Listof Ticket) -> Num
(define (tickets-error-rate fields tickets)
  (for*/sum ([ticket (in-list tickets)]
             [part (in-list ticket)]
             #:unless (valid-for-some-field? fields part))
    part))

(module+ test
  (check-equal? (tickets-error-rate fields nearby) 71))

(module+ stars
  (define-values (fields my-ticket nearby)
    (call-with-input-file "input.txt" parse-input))
  (tickets-error-rate fields nearby))

;;; ---------------------- star 2 --------------------------

;;; all-valid-tickets : (Listof Field) * (Listof Ticket) -> (Listof Ticket)
(define (all-valid-tickets fields tickets)
  (filter (lambda (ticket)
            (andmap (curry valid-for-some-field? fields) ticket))
          tickets))

;;; get-candidates : (Listof Field) * (Listof Ticket) -> (Listof (Listof Index))
(define (get-candidates fields valid-tickets)
  (for/list ([fld (in-list fields)])
    (for/list ([idx (in-range (length fields))]
               #:when (for/and ([ticket (in-list valid-tickets)])
                        (valid-in-field? fld (list-ref ticket idx))))
      idx)))

;;; solve-fields : (Listof Field) * (Listof Ticket) -> (Dict Index Field)
(define (solve-fields fields tickets)
  (define valid-tickets (all-valid-tickets tickets))
  (let loop ([candidates (get-candidates fields valid-tickets)]
             [acc '()])
    (cond
      [(andmap null? candidates) acc]
      [else
       (define field-idx (index-where candidates (lambda (lst) (= 1 (length lst)))))
       (define part-idx (car (list-ref candidates field-idx)))
       (loop (remove-for-all candidates part-idx)
             (dict-set acc part-idx (list-ref fields field-idx)))])))

;;; remove-for-all : (Listof (Listof a)) * a -> (Listof (Listof a))
(define (remove-for-all list-of-lists elem)
  (map (lambda (lst) (remove elem lst))
       list-of-lists))

(module+ test
  (let ([res (solve-fields fields nearby)])
    (check-equal? (field-name (dict-ref res 0)) "row")
    (check-equal? (field-name (dict-ref res 1)) "class")
    (check-equal? (field-name (dict-ref res 2)) "seat")))

(module+ stars
  (define result (solve-fields fields nearby))
  (for/product ([(idx field) (in-dict result)]
                #:when (string-prefix? (field-name field) "departure"))
    (list-ref my-ticket idx)))
