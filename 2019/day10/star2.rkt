#lang racket
(require racket/set
         "star1.rkt")

(module+ test
  (require rackunit))

;; calc-angles: Pos * Map -> (Listof (List Pos angle distance))
(define (calc-angles base map)
  (match-define (pos base-x base-y) base)
  (for/list ([target (in-set map)]
             #:unless (equal? base target))
    (match-define (pos target-x target-y) target)
    (define dx (- target-x base-x))
    (define dy (- target-y base-y))
    (define distance (+ (sqr dx) (sqr dy)))
    (define angle* (+ (atan dy dx) (/ pi 2)))
    (define angle
      (if (< angle* 0) (+ angle* (* pi 2)) angle*))
    (list target angle distance)))

;; group-and-sort: ... -> (Listof (List angle (Listof Pos)))
(define (group-and-sort results)
  (define grouped
    (for/list ([group (in-list (group-by second results))])
      (define angle (second (first group)))
      (define poss
        (map first (sort group < #:key third)))
      (list angle poss)))
  (sort grouped < #:key car))

;; TODO: finish this
(define (sort-asteroids base map)
  (define sorted (group-and-sort (calc-angles base map)))
  'todo)

(module+ test
  (define in
    #<<in
.#..##.###...#######
##.############..##.
.#.######.########.#
.###.#######.####.#.
#####.##.#.##.###.##
..#####..#.#########
####################
#.####....###.#.#.##
##.#################
#####.##.###..####..
..######..##.#######
####.##.####...##..#
.#####..#.######.###
##...#.##########...
#.##########.#######
.####.#.###.###.#.##
....##.##.###..#####
.#.#.###########.###
#.#.#.#####.####.###
###.##.####.##.#..##
in
    )
  (define map (parse-map (open-input-string in)))
  (define base (car (max-detectable-asteroids map)))
  (define sorted (group-and-sort (calc-angles base map))))

(define *order* 200)

(module+ main
  (define map
    (call-with-input-file "input.txt" parse-map))
  (define base (car (max-detectable-asteroids map)))
  (define sorted (group-and-sort (calc-angles base map)))
  ;; the map of input is large enough,
  ;; so the first round can cover the 200th
  (caadr (list-ref sorted (sub1 *order*))))
