#lang racket
(require "../../util.rkt")

(module+ test
  (require rackunit))

;;; Space: All of the cubes (its position and status)
;;;
;;; cubes : (Hash Position Status)
;;; Position = (List Int Int Int) => '(x y z)
;;; Status = Boolean (#t => active / #f => inactive)
;;;
;;; ranges : (List x-min x-max y-min y-max z-min z-max)
;;;   for each cube in this space, its position (x, y, z):
;;;         k-min <= k < k-max, k = x,y,z

(struct space (cubes ranges)
  #:extra-constructor-name make-space)

;;; build-space : String -> Space
(define (build-space plane)
  (define lines (string-split plane "\n"))
  (define cubes
    (for*/hash ([(line y) (in-indexed (in-list lines))]
                [(char x) (in-indexed (in-string line))])
      (values (list x y 0) (char=? char #\#))))
  (define x-max (string-length (first lines)))
  (define y-max (length lines))
  (make-space cubes (list 0 x-max 0 y-max 0 1)))

(module+ test
  (define space (build-space (file->string "test.txt")))
  (check-equal? (space-ranges space)
                '(0 3 0 3 0 1)))

;;; space-ref : Space * Position -> Status
(define (space-ref space position)
  (hash-ref (space-cubes space) position #f))

;;; active-cubes-in-space: Space -> Nat
(define (active-cubes-in-space space)
  (for/count ([v (in-hash-values (space-cubes space))]) v))

;;; cycle : Space -> Space
(define (cycle space)
  (match-define (list x-min x-max y-min y-max z-min z-max)
    (space-ranges space))
  (define new-cubes
    (for*/hash ([x (in-range x-min x-max)]
                [y (in-range y-min y-max)]
                [z (in-range z-min z-max)])
      (define pos (list x y z))
      (values pos (update-cube space pos))))
  (make-space new-cubes (update-ranges (space-ranges space))))

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
  (match-define (list x y z) position)
  (for*/count ([dx (in-list '(-1 0 1))]
               [dy (in-list '(-1 0 1))]
               [dz (in-list '(-1 0 1))]
               #:unless (= dx dy dz 0))
    (space-ref space (list (+ x dx) (+ y dy) (+ z dz)))))

;;; update-ranges : Ranges -> Ranges
;;;   just -1 / +1 each cycle
(define (update-ranges ranges)
  (for/list ([r (in-list ranges)]
             [i (in-naturals)])
    (if (even? i) (sub1 r) (add1 r))))

;;; cycles : Space * Nat -> Space
(define (cycles space times)
  (for/fold ([acc space])
            ([_ (in-range times)])
    (cycle acc)))

(module+ test
  (active-cubes-in-space (cycles space 6)))

(module+ star1
  (define init-space (build-space (file->string "input.txt")))
  (active-cubes-in-space (cycles init-space 6)))
