#lang racket

(module+ test
  (require rackunit))

;; Num = (Cons number (List row col span))
;; Sym = (Cons symbol (List row col))

(define num-number car)
(define num-row cadr)
(define sym-symbol car)
(define sym-row cadr)

;; parse-input : Input-Port -> (Values (Listof Num) (Listof Sym))
(define (parse-input in)
  (port-count-lines! in)
  (let loop ([nums '()] [syms '()])
    (define-values (row col _pos) (port-next-location in))
    (match (read-char in)
      [(? eof-object?)
       (values (reverse nums) (reverse syms))]
      [(or #\newline #\.)
       (loop nums syms)]
      [(? char-numeric? char)
       (loop (cons (read-number in char row col) nums)
             syms)]
      [char
       (define symbol (string->symbol (string char)))
       (loop nums
             (cons (cons symbol (list row col)) syms))])))

;; read-number : Input-Port * Char * row * col -> Num
(define (read-number in fst row start)
  (let loop ([n (digit->number fst)])
    (match (peek-char in)
      [(? char-numeric? char)
       (read-char in)
       (loop (+ (* n 10) (digit->number char)))]
      [_
       (define span (+ (exact-floor (log n 10)) 1))
       (cons n (list row start span))])))

(define (digit->number char)
  (- (char->integer char) (char->integer #\0)))

(module+ test
  (define-values (nums syms)
    (call-with-input-file "test" parse-input)))

(define (group-into-hash key lst)
  (define hash (make-hash))
  (for ([elem (in-list lst)])
    (hash-update! hash (key elem) (lambda (acc) (cons elem acc)) '()))
  hash)

;; adjacent? : Num * Sym -> Boolean
(define (adjacent? num sym)
  (match-define (cons _ (list nrow nstart nspan)) num)
  (match-define (cons _ (list srow scol)) sym)
  (and (<= (- nrow 1) srow (+ nrow 1))
       (<= (- nstart 1) scol (+ nstart nspan))))

;; part-number? : Num * (Hash row (Listof Sym)) -> Boolean
(define (part-number? num syms*)
  (define row (num-row num))
  (for*/or ([r (in-inclusive-range (- row 1) (+ row 1))]
            [sym (in-list (hash-ref syms* r '()))])
    (adjacent? num sym)))

(define (find-part-numbers nums syms)
  (define syms* (group-into-hash sym-row syms))
  (for/list ([num (in-list nums)]
             #:when (part-number? num syms*))
    (num-number num)))

(module+ test
  (check-equal? (find-part-numbers nums syms)
                '(467 35 633 617 592 755 664 598)))

(module+ part1
  (call-with-input-file "input"
    (lambda (in)
      (define-values (nums syms) (parse-input in))
      (apply + (find-part-numbers nums syms)))))

;; gear-ratio : Sym * (Hash row (Listof Num)) -> (U Int #f)
;;   return #f is `sym` is not a gear; else return its gear ratio.
(define (gear-ratio sym nums*)
  (and (eq? (sym-symbol sym) '*)
       (let ()
         (define row (sym-row sym))
         (define adjacent-nums
           (for*/list ([r (in-inclusive-range (- row 1) (+ row 1))]
                       [num (in-list (hash-ref nums* r '()))]
                       #:when (adjacent? num sym))
             (num-number num)))
         (and (= (length adjacent-nums) 2)
              (apply * adjacent-nums)))))

(define (all-gear-ratios nums syms)
  (define nums* (group-into-hash num-row nums))
  (filter-map (lambda (sym) (gear-ratio sym nums*)) syms))

(module+ test
  (check-equal? (all-gear-ratios nums syms) '(16345 451490)))

(module+ part2
  (call-with-input-file "input"
    (lambda (in)
      (define-values (nums syms) (parse-input in))
      (apply + (all-gear-ratios nums syms)))))
