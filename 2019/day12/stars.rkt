#lang racket
(require threading)

(module+ test
  (require rackunit))

;; Star: (List x y z vx vy vz)
(define (make-star x y z)
  (list x y z 0 0 0))

;;; parse
(define star-regexp #px"<x=(-?\\d+), y=(-?\\d+), z=(-?\\d+)>")

(define-match-expander num-str
  (lambda (stx)
    (syntax-case stx ()
      [(str->num id)
       #'(app string->number (? number? id))])))

;; parse-stars: Input-Port -> (Listof Star)
(define (parse-stars in)
  (for/list ([line (in-lines in)])
    (match (regexp-match star-regexp line)
      [(list _ (num-str x) (num-str y) (num-str z))
       (make-star x y z)]
      [else (error "bad input")])))

(module+ test
  (define in
    #<<in
<x=-1, y=0, z=2>
<x=2, y=-10, z=-7>
<x=4, y=-8, z=8>
<x=3, y=5, z=-1>
in
    )
  (define stars (parse-stars (open-input-string in)))
  (check-equal? stars
                (list (make-star -1 0 2)
                      (make-star 2 -10 -7)
                      (make-star 4 -8 8)
                      (make-star 3 5 -1))))

;; NOTE: axes are independent
;; Axis: (Listof (List pos velocity)) with length of 4

(define (stars->axes stars)
  (for/list ([i 3])
    (map (lambda (star) (list (list-ref star i)
                              (list-ref star (+ i 3))))
         stars)))

(define (axes->stars axes)
  (for/list ([i 4])
    (match (map (lambda (a) (list-ref a i)) axes)
      [`((,x ,vx) (,y ,vy) (,z ,vz))
       (list x y z vx vy vz)])))

(define (move-one-step axes)
  (for/list ([axis (in-list axes)])
    (move-one-step/axis axis)))

(define (move-one-step/axis axis)
  (define ps (map first axis))
  (for/list ([star/axis (in-list axis)])
    (match-define (list p v) star/axis)
    (define dv
      (for/sum ([p* (in-list ps)]) (sgn (- p* p))))
    (define new-v (+ v dv))
    (define new-p (+ p new-v))
    (list new-p new-v)))

(define (move-steps stars steps)
  (for/fold ([axes (stars->axes stars)]
             #:result (axes->stars axes))
            ([i (in-range steps)])
    (move-one-step axes)))

(module+ test
  (check-equal? (move-one-step/axis '((-1 0) (2 0) (4 0) (3 0)))
                '((2 3) (3 1) (1 -3) (2 -1))))

(define (total-energy star)
  (* (apply + (map abs (take star 3)))
     (apply + (map abs (drop star 3)))))

(module+ test
  (check-equal? (total-energy (list 2 1 3 -3 -2 1)) 36))

(define (total-energy-sum-after-steps stars steps)
  (~> stars
      (move-steps _ steps)
      (map total-energy _)
      (apply + _)))

(module+ test
  (check-equal? (total-energy-sum-after-steps stars 10)
                179))

(module+ star1
  (call-with-input-file "input.txt"
    (lambda (in)
      (total-energy-sum-after-steps (parse-stars in)
                                    1000))))
