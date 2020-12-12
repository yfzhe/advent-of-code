#lang racket
(require threading
         "../../util.rkt")

(module+ test
  (require rackunit))

;;; GRID: 2d array
;;;   use a vector to represent it, for random indexing

;;; (Gridof a) = (Vectorof a) * Index * Index
(struct grid (data rows cols)
  #:transparent
  #:methods gen:custom-write
  [(define (write-proc grid port mode)
     (for ([y (in-range (grid-rows grid))])
       (for ([x (in-range (grid-cols grid))])
         (write-char (grid-ref grid x y) port))
       (newline port)))])

;;; grid-ref : (Gridof a) * Index * Index -> (U a #f)
(define (grid-ref a-grid x y)
  (match-define (grid data rows cols) a-grid)
  (cond
    [(and (<= 0 x (sub1 cols)) (<= 0 y (sub1 rows)))
     (vector-ref data (+ x (* cols y)))]
    [else #f]))

;;; parse-input : Input-Port -> Grid
(define (parse-input in)
  (define lines (port->lines in))
  (define rows (length lines))
  (define cols (string-length (car lines)))
  (define data (for*/vector ([line (in-list lines)]
                             [char (in-string line)])
                 char))
  (grid data rows cols))

(module+ test
  (define input (call-with-input-file "test.txt" parse-input))
  (check-equal? (grid-ref input 0 0) #\L)
  (check-equal? (grid-ref input 1 0) #\.)
  (check-equal? (grid-ref input 0 1) #\L))

;;; empty & occupied
(define *empty* #\L)
(define *occupied* #\#)

;;; count-occupied : Grid -> Nat
(define (count-occupied grid)
  (for*/count ([y (in-range (grid-rows grid))]
               [x (in-range (grid-cols grid))])
    (eq? *occupied* (grid-ref grid x y))))

;;; ---------------------- transfrom --------------------------

;;; Rule = Grid * Index * Index -> Char
;;; make-transformer : Rule -> (Grid -> Grid)
(define (make-transformer rule)
  (lambda (a-grid)
    (struct-copy grid a-grid
                 [data (for*/vector ([y (in-range (grid-rows a-grid))]
                                     [x (in-range (grid-cols a-grid))])
                         (rule a-grid x y))])))

;;; fix : (a -> a) -> (a -> a)
(define ((fix proc) x)
  (let ([y (proc x)])
    (if (equal? x y) x ((fix proc) y))))

;;; ---------------------- star 1 --------------------------

;;; around-occupied : Grid * Index * Index -> Nat
(define (around-occupied a-grid x y)
  (for*/count ([dx (in-list '(-1 0 1))]
               [dy (in-list '(-1 0 1))]
               #:unless (= dx dy 0))
    (eq? (grid-ref a-grid (+ x dx) (+ y dy))
         *occupied*)))

(module+ test
  (check-equal? (around-occupied input 0 0) 0))

(define transform/1
  (make-transformer
   (lambda (grid x y)
     (define char (grid-ref grid x y))
     (cond
       [(and (eq? char *empty*) (zero? (around-occupied grid x y)))
        *occupied*]
       [(and (eq? char *occupied*) (<= 4 (around-occupied grid x y)))
        *empty*]
       [else char]))))

(module+ test
  (check-equal? (count-occupied ((fix transform/1) input)) 37))

(module+ star1
  (call-with-input-file "input.txt"
    (lambda~> parse-input
              ((fix transform/1) _)
              count-occupied)))

;;; ---------------------- star 2 --------------------------

;;; around-occupied/2 : Grid * Index * Index -> Nat
(define (around-occupied/2 grid x y)
  (for*/count ([dx (in-list '(-1 0 1))]
               [dy (in-list '(-1 0 1))]
               #:unless (= 0 dx dy))
    (let loop ([x x] [y y])
      (define x* (+ x dx))
      (define y* (+ y dy))
      (define here (grid-ref grid x* y*))
      (cond
        [(not here) #f]
        [(eq? here *empty*) #f]
        [(eq? here *occupied*) #t]
        [else (loop x* y*)]))))

(define transform/2
  (make-transformer
   (lambda (grid x y)
     (define char (grid-ref grid x y))
     (cond
       [(and (eq? char *empty*) (zero? (around-occupied/2 grid x y)))
        *occupied*]
       [(and (eq? char *occupied*) (>= (around-occupied/2 grid x y) 5))
        *empty*]
       [else char]))))

(module+ test
  (check-equal? (count-occupied ((fix transform/2) input)) 26))

(module+ star2
  (call-with-input-file "input.txt"
    (lambda~> parse-input
              ((fix transform/2) _)
              count-occupied)))
