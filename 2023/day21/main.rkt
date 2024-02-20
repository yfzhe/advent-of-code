#lang racket

(module+ test
  (require rackunit))

;; ---------- MATRIX ----------
;; (Mat a)
(struct mat (width height slots))

;; make-mat : Index * Index * a -> (Mat a)
(define (make-mat width height u)
  (mat width height (make-vector (* width height) u)))

;; mat-ref : (Mat a) * Index * Index * [a] -> a
(define (mat-ref m x y [failed #f])
  (match-define (mat w h slots) m)
  (cond
    [(or (< x 0) (>= x w)
         (< y 0) (>= y h))
     failed]
    [else (vector-ref slots (+ x (* w y)))]))

;; mat-set! : (Mat a) * Index * Index * a -> Void
(define (mat-set! m x y v)
  (match-define (mat w h slots) m)
  (vector-set! slots (+ x (* w y)) v))

;; ---------- PARSE ----------

;; parse-input : Input-Port -> (Mat Char)
(define (parse-input in)
  (define lines (port->lines in))
  (define width (string-length (car lines)))
  (define height (length lines))
  (define m (make-mat width height #f))
  (for* ([(line y) (in-indexed (in-list lines))]
         [(char x) (in-indexed (in-string line))])
    (mat-set! m x y char))
  m)

(module+ test
  (define m (call-with-input-file "test" parse-input)))

;; ---------- PART 1 ----------
;; Position = (Cons Int Int)

;; start-position : Map -> Position
(define (start-position m)
  (define w (mat-width m))
  (define h (mat-height m))
  (for*/first ([y (in-range h)]
               [x (in-range w)]
               #:when (eq? (mat-ref m x y) #\S))
    (cons x y)))

;; next-positions : Map -> (Listof Position)
(define (next-positions m p)
  (match-define (cons x y) p)
  (for/list ([p* (list (cons (- x 1) y)
                       (cons (+ x 1) y)
                       (cons x (- y 1))
                       (cons x (+ y 1)))]
             #:when (let ([char (mat-ref m (car p*) (cdr p*))])
                      (or (eq? char #\.) (eq? char #\S))))
    p*))

(define (solve/1 m n ps)
  (cond
    [(zero? n) (set-count ps)]
    [else
     (define ps*
       (apply set-union
              (for/list ([p (in-set ps)])
                (list->set (next-positions m p)))))
     (solve/1 m (sub1 n) ps*)]))

(module+ test
  (let ([starts (set (start-position m))])
    (check-equal? (solve/1 m 1 starts) 2)
    (check-equal? (solve/1 m 2 starts) 4)
    (check-equal? (solve/1 m 6 starts) 16)))

(module+ part1
  (call-with-input-file "input"
    (lambda (in)
      (define m (parse-input in))
      (solve/1 m 64 (set (start-position m))))))
