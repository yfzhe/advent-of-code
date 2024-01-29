#lang racket

(require megaparsack megaparsack/text
         data/applicative data/monad)

(module+ test
  (require rackunit))

;; ---------- PARSE ----------
(define newline/p (char/p #\newline))

(define numbers/p
  (do [result <- (many+-until/p integer/p
                                #:sep (char/p #\space)
                                #:end (or/p newline/p (lookahead/p eof/p)))]
      (pure (first result))))

(define converter/p
  (do (many/p letter/p)
      (string/p "-to-")
    (many/p letter/p)
    (string/p " map:")
    newline/p
    [rules <- (many/p numbers/p)]
    (pure rules)))

(define almanac/p
  (do (string/p "seeds: ")
      [seeds <- numbers/p]
    newline/p
    [converters <- (many/p converter/p #:sep newline/p)]
    (pure (list seeds converters))))

;; parse-almanac : Input-Port -> (List (Listof Int) (Listof Converter))
(define (parse-almanac in)
  (parse-result! (parse-string almanac/p (port->string in))))

(module+ test
  (match-define (list seeds converters)
    (call-with-input-file "test" parse-almanac)))

;; ---------- PART 1 ----------

;; convert/1: Converter * Int -> Int
(define (convert/1 converter num)
  (or (for/first ([rule (in-list converter)]
                  #:do [(match-define (list dest src len) rule)]
                  #:when (and (<= src num) (< num (+ src len))))
        (+ dest (- num src)))
      num))

(module+ test
  (let ([converter (first converters)])
    (check-equal? (map (lambda (num) (convert/1 converter num)) seeds)
                  '(81 14 57 13)))

  (check-equal? (foldl convert/1 79 converters) 82))

(module+ part1
  (match-define (list seeds converters)
    (call-with-input-file "input" parse-almanac))
  (apply min
         (for/fold ([nums seeds])
                   ([converter (in-list converters)])
           (map (lambda (num) (convert/1 converter num)) nums))))

;; ---------- PART 2 ----------
(require data/integer-set)

(define (make-range* start len)
  (make-range start (+ start len -1)))

;; build-init-intset: (Listof Integer) -> Integer-Set
(define (build-intset nums)
  (let loop ([acc (make-range)] [nums nums])
    (match nums
      ['() acc]
      [(list start len more ...)
       (loop (union acc (make-range* start len)) more)])))

;; intset-shift: Integer-Set * Int -> Integer-Set
(define (intset-shift intset offset)
  (make-integer-set
   (map (lambda (rng) (cons (+ offset (car rng))
                            (+ offset (cdr rng))))
        (integer-set-contents intset))))

;; convert/2: Converter * Integer-Set -> Integer-Set
(define (convert/2 converter intset)
  (for*/fold ([shifted (make-range)]
              [origin intset]
              #:result (union shifted origin))
             ([rule (in-list converter)])
    (match-define (list dest src len) rule)
    (define-values (inter sub _)
      (split origin (make-range* src len)))
    (values (union (intset-shift inter (- dest src)) shifted)
            sub)))

(module+ part2
  (match-define (list nums converters)
    (call-with-input-file "input" parse-almanac))

  (for*/fold ([intset (build-intset nums)]
              #:result (caar (integer-set-contents intset)))
             ([converter (in-list converters)])
    (convert/2 converter intset)))
