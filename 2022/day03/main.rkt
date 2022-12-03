#lang racket
(require threading)

(module+ test
  (require rackunit))

(define (common-char . strings)
  (~> (map (compose1 list->set string->list) strings)
      (apply set-intersect _)
      set->list
      car))

(define (rucksack-common-item items)
  (define len/2 (/ (string-length items) 2))
  (common-char (substring items 0 len/2)
               (substring items len/2)))

(module+ test
  (check-equal? (rucksack-common-item "vJrwpWtwJgWrhcsFMMfFFhFp")
                #\p))

(define (priority item)
  (cond
    [(char<=? #\a item #\z)
     (+ (- (char->integer item) (char->integer #\a)) 1)]
    [(char<=? #\A item #\Z)
     (+ (- (char->integer item) (char->integer #\A)) 27)]))

(module+ part1
  (call-with-input-file "input"
    (lambda (in)
      (for/sum ([line (in-lines in)])
        (priority (rucksack-common-item line))))))

(module+ part2
  (call-with-input-file "input"
    (lambda (in)
      (for/sum ([lines (in-slice 3 (in-lines in))])
        (priority (apply common-char lines))))))
