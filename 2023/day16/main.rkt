#lang racket
(require threading)

;; Layout = (Listof String) * Int * Int
(struct layout (content width height))

;; parse-input : Input-Port -> Layout
(define (parse-input in)
  (define content (port->lines in))
  (layout content
          (length content)
          (string-length (car content))))

;; layout-ref : Layout * Int * Int -> (U Char #f)
(define (layout-ref l row col)
  (match-define (layout content width height) l)
  (if (or (< row 0) (>= row height)
          (< col 0) (>= col width))
      #f
      (string-ref (list-ref content row) col)))

;; Position = (cons Int Int)
;; Direction = (cons Int Int)
(define up '(-1 . 0))
(define down '(1 . 0))
(define left '(0 . -1))
(define right '(0 . 1))

;; next-directions : Char * Direction -> (Listof Direction)
(define (next-directions char dir)
  (match char
    [#f '()]
    [#\. (list dir)]
    [#\\ (match dir
           [(== up) (list left)]
           [(== left) (list up)]
           [(== down) (list right)]
           [(== right) (list down)])]
    [#\/ (match dir
           [(== up) (list right)]
           [(== right) (list up)]
           [(== down) (list left)]
           [(== left) (list down)])]
    [#\| (match dir
           [(== up) (list up)]
           [(== down) (list down)]
           [_ (list up down)])]
    [#\- (match dir
           [(== left) (list left)]
           [(== right) (list right)]
           [_ (list left right)])]))

;; energize-tiles : Layout * Position * Direction -> Int
(define (energize-tiles layout pos dir)
  ;; acc : (Setof (cons Position Direction))
  ;; sorry for mutation here
  (define acc (mutable-set))
  (do-one-tile layout acc pos dir)
  (set-count (list->set (set-map acc car))))

;; do-one-tile : Layout * acc * Position * Direction -> Void
(define (do-one-tile layout acc pos dir)
  (define char (layout-ref layout (car pos) (cdr pos)))
  (cond
    [(not char) (void)]
    [(set-member? acc (cons pos dir)) (void)]
    [else
     (set-add! acc (cons pos dir))
     (define dirs* (next-directions char dir))
     (for ([dir* (in-list dirs*)])
       (define pos* (cons (+ (car pos) (car dir*))
                          (+ (cdr pos) (cdr dir*))))
       (do-one-tile layout acc pos* dir*))]))

(module+ part1
  (~> (call-with-input-file "input" parse-input)
      (energize-tiles _ '(0 . 0) right)))

;; max-energized : Layout * (Sequenceof Int) * (Sequenceof Int) * Direction -> Int
(define (max-energized layout rows cols init-dir)
  (for*/fold ([acc 0])
             ([row rows] [col cols])
    (max acc
         (energize-tiles layout (cons row col) init-dir))))

(module+ part2
  (define layout (call-with-input-file "input" parse-input))

  (define height (layout-height layout))
  (define width (layout-width layout))

  (max (max-energized layout (list 0)            (in-range 0 width) down)
       (max-energized layout (list (- height 1)) (in-range 0 width) up)
       (max-energized layout (in-range 0 height) (list 0)           right)
       (max-energized layout (in-range 0 height) (list (- width 1)) left)))
