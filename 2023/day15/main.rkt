#lang racket
(require threading)

(module+ test
  (require rackunit))

;; parse-input : Input-Port -> (Listof String)
(define (parse-input in)
  (~> (read-line in)
      (string-split _ ",")))

;; ---------- PART 1 ----------
;; hash-code : String -> Int
(define (hash-code str)
  (for/fold ([acc 0])
            ([char (in-string str)])
    (~> (char->integer char)
        (+ acc _)
        (* _ 17)
        (remainder _ 256))))

(module+ test
  (check-equal? (hash-code "rn=1") 30)
  (check-equal? (hash-code "cm-") 253)
  (check-equal? (hash-code "qp=3") 97))

(module+ part1
  (define steps (call-with-input-file "input" parse-input))
  (for/sum ([str (in-list steps)])
    (hash-code str)))

;; ---------- PART 2 ----------
;; Box = (Dict label num)
;; Boxes = (Vector Dict 256)

;; do-steps : (Listof String) -> Boxes
(define (do-steps steps)
  (define boxes (make-vector 256 '()))
  (for ([step (in-list steps)])
    (do-step boxes step))
  boxes)

;; do-step : Boxes * String -> Void
(define (do-step boxes step)
  (match-define (list _ left op right)
    (regexp-match #rx"(.+)([=-])(.*)" step))
  (define idx (hash-code left))
  (define n (string->number right))
  (match op
    ["=" (vector-update! boxes idx
                         (lambda (box) (dict-set box left n)))]
    ["-" (vector-update! boxes idx
                         (lambda (box) (dict-remove box left)))]))

;; vector-update! : (Vectorof a) * Index * (a -> a) -> (Vectorof a)
(define (vector-update! vec idx proc)
  (vector-set! vec idx (proc (vector-ref vec idx))))

(module+ test
  (define boxes (call-with-input-file "test" (lambda~> parse-input do-steps)))
  (check-equal? (vector-ref boxes 0) '(("rn" . 1) ("cm" . 2)))
  (check-equal? (vector-ref boxes 1) '())
  (check-equal? (vector-ref boxes 2) '())
  (check-equal? (vector-ref boxes 3) '(("ot" . 7) ("ab" . 5) ("pc" . 6))))

;; focusing-power : Boxes -> Int
(define (focusing-power boxes)
  (for*/sum ([(box box-idx) (in-indexed (in-vector boxes))]
             [(len slot-idx) (in-indexed (in-dict-values box))])
    (* (+ box-idx 1) (+ slot-idx 1) len)))

(module+ part2
  (call-with-input-file "input"
    (lambda~> parse-input
              do-steps
              focusing-power)))
