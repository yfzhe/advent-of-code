#lang racket/base
(require racket/string racket/match racket/set)

(module+ test
  (require rackunit))

;; Wire: (Listof Wire-Node)
;; Wire-Node: (Cons (U #\U #\D #\L #\R) ; direction
;;                  Number)

(define (parse-wire-path str)
  (map parse-wire-node (string-split str ",")))

(define (parse-wire-node node)
  (define direction (string-ref node 0))
  (define length (substring node 1))
  (cons direction (string->number length)))

(module+ test
  (check-equal? (parse-wire-path "R8,U5,L5,D3")
                '((#\R . 8) (#\U . 5) (#\L . 5) (#\D . 3))))

;; Pos: (Cons Integer Integer)

(define *central-port* '(0 . 0))

;; make-pos-updater:
;; something like a lens
(define (make-pos-updater field updater)
  (lambda (pos)
    (case field
      [(x) (cons (updater (car pos)) (cdr pos))]
      [(y) (cons (car pos) (updater (cdr pos)))])))

;; print-wire: Wire -> (Setof Pos)
;; use a set as a canvas, and print **each** point of the wire on it
(define (print-wire wire)
  (for/fold ([base '(0 . 0)]
             [canvas (set)]
             #:result canvas)
            ([node (in-list wire)])
    (define amount (cdr node))
    (define updater
      (match (car node)
        [#\U (make-pos-updater 'y add1)]
        [#\D (make-pos-updater 'y sub1)]
        [#\L (make-pos-updater 'x sub1)]
        [#\R (make-pos-updater 'x add1)]))
    
    (for/fold ([pos base]
               [canvas canvas])
              ([_ (in-range amount)])
      (let ([point (updater pos)])
        (values point
                (set-add canvas (updater pos)))))))

(define (get-answer wire1 wire2)
  (define crosses
    (set-intersect (print-wire wire1)
                   (print-wire wire2)))
  (for/fold ([acc +inf.0]
             #:result (inexact->exact acc))
            ([cross (in-set crosses)])
    (min acc
         (+ (abs (car cross)) (abs (cdr cross))))))

(module+ test
  (check-equal? (get-answer (parse-wire-path "R75,D30,R83,U83,L12,D49,R71,U7,L72")
                            (parse-wire-path "U62,R66,U55,R34,D71,R55,D58,R83"))
                159))

(module+ main
  (call-with-input-file "input.txt"
    (lambda (in)
      (define wire1 (parse-wire-path (read-line in)))
      (define wire2 (parse-wire-path (read-line in)))
      (get-answer wire1 wire2))))

