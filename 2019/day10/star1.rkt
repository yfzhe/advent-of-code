#lang racket
(require racket/set threading)

(module+ test
  (require rackunit))

(struct pos (x y) #:transparent)

;; Map : (Setof Pos)

(define (parse-map in)
  (for*/set ([(line y) (in-indexed (in-lines in))]
             [(char x) (in-indexed (in-string line))]
             #:when (char=? char #\#))
    (pos x y)))

(module+ test
  (check-equal? (parse-map (open-input-string ".#\n#.\n"))
                (set (pos 0 1) (pos 1 0))))

(define (detectable? pos1 pos2 map)
  (match-define (pos pos1-x pos1-y) pos1)
  (match-define (pos pos2-x pos2-y) pos2)
  (cond
    [(and (= pos1-x pos2-x) (= pos1-y pos2-y)) #f]
    [(> pos1-x pos2-x)
     (detectable? pos2 pos1 map)] ; keep pos1 is on the left of pos2
    [(= pos1-x pos2-x)
     (define lower (min pos1-y pos2-y))
     (define higher (max pos1-y pos2-y))
     (not
      (for/or ([y (in-range (add1 lower) higher)])
        (set-member? map (pos pos1-x y))))]
    [else
     (define k (/ (- (pos-y pos1) (pos-y pos2))
                  (- (pos-x pos1) (pos-x pos2))))

     (not
      (for/or ([x (in-range (add1 pos1-x) pos2-x)])
        (define y (+ (* k (- x pos1-x)) pos1-y))
        (set-member? map (pos x y))))]))

(module+ test
  (define map
    (set (pos 0 0) (pos 1 2) (pos 2 1) (pos 4 2) (pos 2 4)
         (pos 0 1) (pos 0 2) (pos 1 0) (pos 2 0)))

  (check-false (detectable? (pos 0 0) (pos 4 2) map))
  (check-false (detectable? (pos 4 2) (pos 0 0) map))
  (check-true (detectable? (pos 0 0) (pos 2 1) map))
  (check-false (detectable? (pos 0 0) (pos 2 4) map))

  (check-false (detectable? (pos 0 0) (pos 0 0) map))
  (check-true (detectable? (pos 1 2) (pos 2 1) map))

  (check-true (detectable? (pos 0 0) (pos 0 1) map))
  (check-false (detectable? (pos 0 0) (pos 2 0) map))
  (check-false (detectable? (pos 0 0) (pos 0 2) map)))

(define (detect-asteroids pos map)
  (for/sum ([target (in-set map)]
            #:when (detectable? pos target map))
    1))

;; max-detectable-asteroids: Map -> (cons Pos Integer)
(define (max-detectable-asteroids map)
  (for/fold ([acc-pos #f]
             [acc-count 0]
             #:result (cons acc-pos acc-count))
            ([pos (in-set map)])
    (define count (detect-asteroids pos map))
    (if (> count acc-count)
        (values pos count)
        (values acc-pos acc-count))))

(define get-answer
  (lambda~> parse-map
            max-detectable-asteroids
            cdr))

(module+ test
  (test-case "full test 1"
    (define input
      #<<in
......#.#.
#..#.#....
..#######.
.#.#.###..
.#..#.....
..#....#.#
#..#....#.
.##.#..###
##...#..#.
.#....####
in
      )
    (check-equal? (get-answer (open-input-string input))
                  33)))

(module+ main
  (call-with-input-file "input.txt"
    get-answer))

(provide (struct-out pos)
         parse-map
         detectable?
         max-detectable-asteroids)
