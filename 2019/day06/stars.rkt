#lang racket/base
(require racket/list racket/string racket/match)

(module+ test
  (require rackunit))

;; Object: String
;; Map: (Hash [around : Object] [base : Object])

(define COM "COM")

(define (parse-map input)
  (for/hash ([line (in-lines input)])
    (match-define (list base around)
      (string-split line ")"))
    (values around base)))

(define (find-path-to-COM map obj)
  (let loop ([target obj] [acc '()])
    (cond
      [(not target) acc]
      [else
       (loop (hash-ref map target #f)
             (cons target acc))])))

(define (get-depth map obj)
  (sub1 (length (find-path-to-COM map obj))))

(module+ test
  (define input
    #<<in
COM)B
B)C
C)D
D)E
E)F
B)G
G)H
D)I
E)J
J)K
K)L
in
    )

  (define map (parse-map (open-input-string input)))

  (check-equal? (get-depth map "D") 3)
  (check-equal? (get-depth map "L") 7))

;;;--------------------------------------------------
;;; part 1
(define (get-answer/1 map)
  (for/sum ([obj (in-hash-keys map)])
    (get-depth map obj)))

(module+ test
  (check-equal? (get-answer/1 map) 42))

(module+ star1
  (call-with-input-file "input.txt"
    (lambda (in)
      (get-answer/1 (parse-map in)))))

;;;--------------------------------------------------
;;; part 2
(define (get-answer/2 map)
  (define COM-to-YOU (find-path-to-COM map "YOU"))
  (define COM-to-SAN (find-path-to-COM map "SAN"))
  (define-values (path-Y path-S)
    (drop-common-prefix COM-to-YOU COM-to-SAN))
  (+ (length path-Y) (length path-S) -2))

(module+ test
  (define input/2 (string-append input "\nK)YOU\nI)SAN"))
  (define map/2 (parse-map (open-input-string input/2)))
  
  (check-equal? (get-answer/2 map/2) 4))

(module+ star2
  (call-with-input-file "input.txt"
    (lambda (in)
      (get-answer/2 (parse-map in)))))

