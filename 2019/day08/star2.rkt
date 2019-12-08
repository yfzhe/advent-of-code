#lang racket/base

(module+ test
  (require rackunit))

(require "star1.rkt")

(define (decode-image layers)
  (apply map
         (lambda pixels
           (ormap (lambda (p) (and (< p 2) p))
                  pixels))
         layers))

(define (print-image image)
  (for ([i (in-range *height*)])
    (for ([j (in-range *width*)])
      (define pixel (list-ref image (+ j (* i *width*))))
      (display (if (zero? pixel) "█" " ")))
    (newline)))

(module+ test
  (check-equal? (decode-image '((0 2 2 2) (1 1 2 2) (2 2 1 2) (0 0 0 0)))
                '(0 1 1 0)))

(module+ main
  (call-with-input-file "input.txt"
    (lambda (in)
      (define input (parse-input (read-line in)))
      (define layers (get-layers input))
      (define image (decode-image layers))
      (print-image image))))

