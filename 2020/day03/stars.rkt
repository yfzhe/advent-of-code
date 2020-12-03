#lang racket
(require threading)

(module+ test
  (require rackunit))

;;; tree-map : Nat * Nat * (Setof (Cons Nat Nat))
(struct tree-map (width height trees)
  #:transparent)

;;; parse-tree-map : Input-Port -> Tree-Map
(define (parse-tree-map in)
  (define lines (port->list read-line in))

  (tree-map (string-length (first lines))
            (length lines)
            (for*/set ([(line y) (in-indexed (in-list lines))]
                       [(char x) (in-indexed (in-string line))]
                       #:when (eq? char #\#))
              (cons x y))))

(module+ test
  (check-equal? (parse-tree-map (open-input-string "..##\n#.#."))
                (tree-map 4
                          2
                          (set '(2 . 0) '(3 . 0) '(0 . 1) '(2 . 1)))))

;;; traverse-map : Tree-Map * Nat * Nat -> Nat
(define (traverse-map map right down)
  (match-define (tree-map width height trees) map)
  (let loop ([x 0] [y 0] [acc 0])
    (cond
      [(> y height) acc]
      [(set-member? trees (cons (modulo x width) y))
       (loop (+ x right)
             (+ y down)
             (add1 acc))]
      [else
       (loop (+ x right)
             (+ y down)
             acc)])))


(module+ test
  (define input #<<EOF
..##.......
#...#...#..
.#....#..#.
..#.#...#.#
.#...##..#.
..#.##.....
.#.#.#....#
.#........#
#.##...#...
#...##....#
.#..#...#.#
EOF
    )
  (define map (parse-tree-map (open-input-string input)))
  (check-equal? (traverse-map map 3 1) 7))

(module+ star1
  (call-with-input-file "input.txt"
    (lambda~> parse-tree-map
              (traverse-map _ 3 1))))

(define *slopes*
  '((1 1)
    (3 1)
    (5 1)
    (7 1)
    (1 2)))

(module+ test
  (check-equal? (traverse-map map 1 1) 2)
  (check-equal? (traverse-map map 5 1) 3)
  (check-equal? (traverse-map map 7 1) 4)
  (check-equal? (traverse-map map 1 2) 2))

(module+ star2
  (call-with-input-file "input.txt"
    (lambda (in)
      (define map (parse-tree-map in))
      (for/product ([slope (in-list *slopes*)])
        (apply traverse-map map slope)))))
