#lang racket
(require threading)
(require "../../util.rkt")

(module+ test
  (require rackunit))

;;; Space: All of the cubes (its position and status)
;;;
;;; cubes : (Hash Position Status)
;;; Position = (List Int Int Int Int) => '(x y z w)
;;; Status = Boolean (#t => active / #f => inactive)
;;;
;;; ranges : (List x-min x-max y-min y-max z-min z-max w-min w-max)
;;;   for each cube in this space, its position (x, y, z, w):
;;;         k-min <= k < k-max, k = x,y,z,w
;;;
;;; dimensions: Nat

(struct space (cubes ranges dimensions)
  #:extra-constructor-name make-space)

;;; build-space : String -> Space
(define (build-space plane dim)
  (define lines (string-split plane "\n"))
  (define cubes
    (for*/hash ([(line y) (in-indexed (in-list lines))]
                [(char x) (in-indexed (in-string line))])
      (values (list x y 0 0) (char=? char #\#))))
  (define x-max (string-length (first lines)))
  (define y-max (length lines))
  (make-space cubes (list 0 x-max 0 y-max 0 1 0 1) dim))

(module+ test
  (define space3 (build-space (file->string "test.txt") 3))
  (define space4 (build-space (file->string "test.txt") 4))
  (check-equal? (space-ranges space3)
                '(0 3 0 3 0 1 0 1)))

;;; space-ref : Space * Position -> Status
(define (space-ref space position)
  (hash-ref (space-cubes space) position #f))

;;; active-cubes-in-space: Space -> Nat
(define (active-cubes-in-space space)
  (for/count ([v (in-hash-values (space-cubes space))]) v))

;;; cycle : Space -> Space
(define (cycle space)
  (define new-ranges (update-ranges space))
  (match-define (list x-min x-max y-min y-max z-min z-max w-min w-max)
    new-ranges)
  (define new-cubes
    (for*/hash ([x (in-range x-min x-max)]
                [y (in-range y-min y-max)]
                [z (in-range z-min z-max)]
                [w (in-range w-min w-max)])
      (define pos (list x y z w))
      (values pos (update-cube space pos))))
  (make-space new-cubes new-ranges (space-dimensions space)))

;;; update-cube : Space * Position -> Status
(define (update-cube space pos)
  (define here (space-ref space pos))
  (define around-actives (active-cubes-around space pos))
  (match* (here around-actives)
    [(#t (or 2 3)) #t]
    [(#f 3) #t]
    [(_ _) #f]))

;;; active-cubes-around : Space * Position -> Nat
(define (active-cubes-around space position)
  (match-define (list x y z w) position)
  (define dws
    (case (space-dimensions space)
      [(3) '(0)]
      [(4) '(-1 0 1)]))
  (for*/count ([dx (in-list '(-1 0 1))]
               [dy (in-list '(-1 0 1))]
               [dz (in-list '(-1 0 1))]
               [dw (in-list dws)]
               #:unless (= dx dy dz dw 0))
    (space-ref space (list (+ x dx) (+ y dy) (+ z dz) (+ w dw)))))

;;; update-ranges : Space -> Ranges
;;;   just -1 / +1 each cycle,
;;;   if the space is 3-dimensions, skip the "w" dimension
(define (update-ranges sp)
  (match-define (space _ ranges dimensions) sp)
  (for/list ([r (in-list ranges)]
             [i (in-naturals)])
    (cond
      [(>= i (* 2 dimensions)) r]
      [(even? i) (sub1 r)]
      [else (add1 r)])))

(module+ test
  (check-equal? (update-ranges space3)
                '(-1 4 -1 4 -1 2 0 1))
  (check-equal? (update-ranges space4)
                '(-1 4 -1 4 -1 2 -1 2)))

;;; cycles : Space * Nat -> Space
(define (cycles space times)
  (for/fold ([acc space])
            ([_ (in-range times)])
    (cycle acc)))

;;; get-result: group these together
(define (get-result space)
  (active-cubes-in-space (cycles space 6)))

(module+ test
  (check-equal? (get-result space3) 112)
  (check-equal? (get-result space4) 848))

(module+ star1
  (~> (file->string "input.txt")
      (build-space _ 3)
      get-result))

(module+ star2
  (~> (file->string "input.txt")
      (build-space _ 4)
      get-result))
