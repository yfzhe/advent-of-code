#lang racket

(module+ test
  (require rackunit))

(struct star (x y z vx vy vz)
  #:mutable
  #:transparent)

(define (make-star x y z)
  (star x y z 0 0 0))

;;; parse
(define star-regexp #px"<x=(-?\\d+), y=(-?\\d+), z=(-?\\d+)>")

(define-match-expander str->num
  (lambda (stx)
    (syntax-case stx ()
      [(str->num id)
       #'(app string->number (? number? id))])))

;; parse-stars: Input-Port -> (Listof Star)
(define (parse-stars in)
  (for/list ([line (in-lines in)])
    (match (regexp-match star-regexp line)
      [(list _ (str->num x) (str->num y) (str->num z))
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

(define (update-velocity star1 star2 pos-getter v-getter v-setter)
  (define p1 (pos-getter star1))
  (define p2 (pos-getter star2))
  (cond
    [(= p1 p2) (void)]
    [(< p1 p2)
     (v-setter star1 (add1 (v-getter star1)))
     (v-setter star2 (sub1 (v-getter star2)))]
    [(> p1 p2)
     (v-setter star1 (sub1 (v-getter star1)))
     (v-setter star2 (add1 (v-getter star2)))]))

(define (update-velocities/2star2 star1 star2)
  (update-velocity star1 star2 star-x star-vx set-star-vx!)
  (update-velocity star1 star2 star-y star-vy set-star-vy!)
  (update-velocity star1 star2 star-z star-vz set-star-vz!))

(define (update-position s)
  (match-define (star x y z vx vy vz) s)
  (set-star-x! s (+ x vx))
  (set-star-y! s (+ y vy))
  (set-star-z! s (+ z vz)))

(define (move-once stars)
  (for ([2stars (in-combinations stars 2)])
    (apply update-velocities/2star2 2stars))

  (for-each update-position stars))

(module+ test
  #;(begin (move-once stars) stars))

(define (total-energy s)
  (match-define (star x y z vx vy vz) s)
  (* (+ (abs x) (abs y) (abs z))
     (+ (abs vx) (abs vy) (abs vz))))

(module+ test
  (check-equal? (total-energy (star 2 1 3 -3 -2 1)) 36))

(module+ main
  (define stars
    (call-with-input-file "input.txt" parse-stars))
  (for ([i (in-range 1000)])
    (move-once stars))
  (for/sum ([star (in-list stars)])
    (total-energy star)))

(provide (struct-out star)
         parse-stars
         move-once)
