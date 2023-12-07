#lang racket
(require threading)

(module+ test
  (require rackunit))

;; ways-to-win:
;;   the numbers of integer x where x * (t - x) >= n
;;   (x - t/2)^2 <= (t/2)^2 - n
(define (ways-to-win t n)
  (define t/2 (/ t 2))
  (define d (- (sqr t/2) n))
  (if (< d 0)
      0
      (let ([r (sqrt d)])
        (- (exact-ceiling (+ t/2 r)) (exact-floor (- t/2 r)) 1))))

(module+ test
  (check-equal? (ways-to-win 7 9) 4)
  (check-equal? (ways-to-win 15 40) 8)
  (check-equal? (ways-to-win 30 200) 9))

(define (parse-input/1 in)
  (for/list ([line (in-lines in)])
    (~> (string-split line)
        cdr
        (map string->number _))))

(module+ part1
  (~> (call-with-input-file "input" parse-input/1)
      (apply map ways-to-win _)
      (apply * _)))

(define (parse-input/2 in)
  (for/list ([line (in-lines in)])
    (~> (string-split line)
        cdr
        (string-join _ "")
        string->number)))

(module+ part2
  (~> (call-with-input-file "input" parse-input/2)
      (apply ways-to-win _)))
