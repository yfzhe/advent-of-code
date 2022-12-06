#lang racket
(require threading)

(module+ test
  (require rackunit))

(define (parse-input in)
  (match-define (list stacks ops)
    (string-split (port->string in) "\n\n"))
  (values (parse-stacks stacks)
          (parse-ops ops)))

(define (parse-stacks stacks)
  (define lines (reverse (string-split stacks "\n")))
  (define keys (string-split (car lines)))
  (for/fold ([dict (map (lambda (key) (cons key '())) keys)])
            ([line (in-list (cdr lines))])
    (for/list ([i (in-naturals)]
               [(key stack) (in-dict dict)])
      (define item
        (let ([idx (+ (* i 4) 1)])
          (if (>= idx (string-length line))
              #f
              (let ([char (string-ref line idx)])
                (if (equal? char #\space) #f char)))))

      (cons key (if item (cons item stack) stack)))))

(define (parse-ops ops)
  (for/list ([line (in-list (string-split ops "\n"))])
    (match (regexp-match #px"move (\\d+) from (\\d+) to (\\d+)" line)
      [(list _ num from to)
       (list (string->number num) from to)])))

(module+ test
  (define-values (stacks ops)
    (call-with-input-file "test" parse-input))

  (check-equal? stacks
                '(("1" . (#\N #\Z))
                  ("2" . (#\D #\C #\M))
                  ("3" . (#\P)))))

;; mode: 'single | 'multiple
(define (do-op stacks op #:mode mode)
  (match-define (list num from to) op)

  (define from-stack (dict-ref stacks from))
  (define to-stack (dict-ref stacks to))

  (define modef
    (case mode [(single) reverse] [(multiple) identity]))

  (~> stacks
      (dict-set _ from (drop from-stack num))
      (dict-set _ to (append (modef (take from-stack num)) to-stack))))

(define (do-ops stacks ops #:mode mode)
  (foldl (lambda (op stacks) (do-op stacks op #:mode mode))
         stacks ops))

(define (answer stacks ops #:mode mode)
  (~> (do-ops stacks ops #:mode mode)
      dict-values
      (map car _)
      list->string))

(module+ test
  (check-equal? (answer stacks ops #:mode 'single) "CMZ")
  (check-equal? (answer stacks ops #:mode 'multiple) "MCD"))

(module+ part1
 (call-with-input-file "input"
   (lambda (in)
     (define-values (stacks ops) (parse-input in))
     (answer stacks ops #:mode 'single))))

(module+ part2
 (call-with-input-file "input"
   (lambda (in)
     (define-values (stacks ops) (parse-input in))
     (answer stacks ops #:mode 'multiple))))
