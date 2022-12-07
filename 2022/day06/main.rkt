#lang racket

(module+ test
  (require rackunit))

(define (solve buffer window-size)
  (let loop ([chars (string->list buffer)]
             [idx 0])
    (cond
      [(check-duplicates (take chars window-size))
       (loop (cdr chars) (add1 idx))]
      [else (+ idx window-size)])))

(module+ test
  (check-equal? (solve "mjqjpqmgbljsphdztnvjfqwrcgsmlb" 4) 7)
  (check-equal? (solve "bvwbjplbgvbhsrlpgdmjqwftvncz" 4) 5)

  (check-equal? (solve "mjqjpqmgbljsphdztnvjfqwrcgsmlb" 14) 19)
  (check-equal? (solve "bvwbjplbgvbhsrlpgdmjqwftvncz" 14) 23))

(module+ part1
 (call-with-input-file "input"
   (lambda (in)
     (solve (port->string in) 4))))

(module+ part2
 (call-with-input-file "input"
   (lambda (in)
     (solve (port->string in) 14))))
