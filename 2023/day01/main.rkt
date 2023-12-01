#lang racket

(module+ test
  (require rackunit))

;; extractor = string * number -> (U number #f)

;; extract-number/1 : extractor
(define (extract-number/1 line idx)
  (define char (string-ref line idx))
  (if (char-numeric? char)
      (- (char->integer char) 48)
      #f))

(define (calibration-value line extractor)
  (define nums
    (filter-map (lambda (idx) (extractor line idx))
                (range 0 (string-length line))))
  (+ (* 10 (first nums)) (last nums)))

(module+ test
  (check-equal? (calibration-value "1abc2" extract-number/1) 12)
  (check-equal? (calibration-value "pqr3stu8vwx" extract-number/1) 38))

(module+ part1
  (call-with-input-file "input"
    (lambda (in)
      (for/sum ([line (in-lines in)])
        (calibration-value line extract-number/1)))))

(define *numbers*
  '("one" "two" "three" "four" "five" "six" "seven" "eight" "nine"))

;; extract-number/2 : extractor
(define (extract-number/2 line idx)
  (or (extract-number/1 line idx)
      (let ([substr (substring line idx)])
        (for/first ([num *numbers*] [n (in-naturals 1)]
                    #:when (string-prefix? substr num))
          n))))

(module+ test
  (let ([extractor extract-number/2])
    (check-equal? (calibration-value "two1nine" extractor) 29)
    (check-equal? (calibration-value "eightwothree" extractor) 83)
    (check-equal? (calibration-value "abcone2threexyz" extractor) 13)))

(module+ part2
  (call-with-input-file "input"
    (lambda (in)
      (for/sum ([line (in-lines in)])
        (calibration-value line extract-number/2)))))
