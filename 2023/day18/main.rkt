#lang typed/racket
(require threading)

(module+ test
  (require typed/rackunit))

;; references:
;; Shoelace Formula: https://en.wikipedia.org/wiki/Shoelace_formula
;; Pick's Theorem: https://en.wikipedia.org/wiki/Pick%27s_theorem

(define-type Point (Pairof Integer Integer))
(define-type Line (List Point Point))
(define-type Polygon (Listof Line))

(: perimeter (-> Polygon Number))
(define (perimeter polygon)
  (for/sum ([line (in-list polygon)])
    (match-define (list (cons x1 y1) (cons x2 y2)) line)
    (+ (abs (- x1 x2)) (abs (- y1 y2)))))

(: area (-> Polygon Number))
(define (area polygon)
  (/ (for/sum ([line (in-list polygon)]) : Integer
       (match-define (list (cons x1 y1) (cons x2 y2)) line)
       (- (* x1 y2) (* x2 y1)))
     2))

(: integer-points (-> Polygon Number))
(define (integer-points polygon)
  (define a (area polygon))
  (define p (perimeter polygon))
  (define interiors (- a (/ p 2) -1))
  (+ p interiors))

;; ---------- PART 1 & 2 ----------

(define-type Plan (List Symbol Integer String))

(: parse-input (-> Input-Port (Listof Plan)))
(define (parse-input in)
  (for/list ([line (in-lines in)])
    (match (string-split line)
      [(list dir num hex)
       (list (string->symbol dir)
             (cast (string->number num) Integer)
             hex)])))

(: make-polygon (-> (Listof Plan) Polygon))
(define (make-polygon plans)
  (for/fold ([p : Point '(0 . 0)]
             [acc : (Listof Line) '()]
             #:result acc)
            ([plan (in-list plans)])
    (match-define (list dir num _) plan)
    (match-define (cons x y) p)
    (define p*
      (match dir
        ['R (cons (+ x num) y)]
        ['L (cons (- x num) y)]
        ['U (cons x (- y num))]
        ['D (cons x (+ y num))]))
    (values p* (cons (list p p*) acc))))

(module+ test
  (define input (call-with-input-file "test" parse-input))
  (check-equal? (integer-points (make-polygon input)) 62))

(module+ part1
  (~> (call-with-input-file "input" parse-input)
      make-polygon
      integer-points))

(: hex->plan (-> Plan Plan))
(define (hex->plan plan)
  (define hex (third plan))
  (list (match (string-ref hex 7)
          [#\0 'R] [#\1 'D] [#\2 'L] [#\3 'U])
        (cast (string->number (substring hex 2 7) 16) Integer)
        ""))

(module+ part2
  (~> (call-with-input-file "input" parse-input)
      (map hex->plan _)
      make-polygon
      integer-points))
