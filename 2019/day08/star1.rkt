#lang racket/base
(require racket/list)

(module+ test
  (require rackunit))

(define *width* 25)
(define *height* 6)

(define layer-size (* *width* *height*))

;; Image: (Listof Integer)

(define (parse-input input-string)
  (for/list ([digit (in-string input-string)])
    (- (char->integer digit) 48)))

(module+ test
  (check-equal? (parse-input "123456789012")
                '(1 2 3 4 5 6 7 8 9 0 1 2)))

(define (get-layers image)
  (let loop ([acc '()] [image image])
    (cond
      [(empty? image) (reverse acc)]
      [else
       (let-values ([(layer remain)
                     (split-at image layer-size)])
         (loop (cons layer acc)
               remain))])))

(provide *width* *height*
         parse-input get-layers)

(module+ main
  (call-with-input-file "input.txt"
    (lambda (in)
      (define image (parse-input (read-line in)))
      (define layers (get-layers image))
      
      (define target-layer
        (for/fold ([acc-zeros +inf.0]
                   [acc-layer #f]
                   #:result acc-layer)
                  ([layer (in-list layers)])
          (define zeros (count zero? layer))
          (if (< zeros acc-zeros)
              (values zeros layer)
              (values acc-zeros acc-layer))))

      (define ones (count (λ (x) (= x 1)) target-layer))
      (define twos (count (λ (x) (= x 2)) target-layer))

      (* ones twos))))
