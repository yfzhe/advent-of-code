#lang racket
(require threading)

(module+ test
  (require rackunit))

(define (parse-input in)
  (for/list ([line (in-lines in)])
    (~> (string-split line)
        (map string->number _))))

(define (safe?/1 report)
  (define sign (sgn (- (second report) (first report))))
  (let loop ([last (first report)]
             [more (rest report)])
    (cond
      [(null? more) #t]
      [(<= 1 (* sign (- (car more) last)) 3)
       (loop (car more) (cdr more))]
      [else #f])))

(module+ test
  (define reports
    (call-with-input-file "test" parse-input))

  (check-equal? (map safe?/1 reports)
                '(#t #f #f #f #f #t)))

(module+ part1
  (~> (call-with-input-file "input" parse-input)
      (count safe?/1 _)))

(define (safe?/2 report)
  (or (safe?/1 report)
      (for/or ([i (in-range 0 (length report))])
        (safe?/1 (remove-at report i)))))

(define (remove-at lst pos)
  (append (take lst pos) (drop lst (add1 pos))))

(module+ test
  (check-equal? (map safe?/2 reports)
                '(#t #f #f #t #t #t)))

(module+ part2
  (~> (call-with-input-file "input" parse-input)
      (count safe?/2 _)))
