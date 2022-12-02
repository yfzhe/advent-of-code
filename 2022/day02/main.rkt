#lang racket
(require threading)

;;; choice = 'rock | 'paper | 'scissors
;;; result = 'win | 'lose | 'draw

(define *choices* '(rock paper scissors))

(define (parse-input in)
  (for/list ([line (in-lines in)])
    (list (string-ref line 0)
          (string-ref line 2))))

;; ---------- PART 1 ----------

(define (interp-choice char)
  (match char
    [(or #\A #\X) 'rock]
    [(or #\B #\Y) 'paper]
    [(or #\C #\Z) 'scissors]))

(define (outcome player opponent)
  (match* (player opponent)
    [(_ _) #:when (eq? player opponent) 'draw]
    [('rock 'scissors) 'win]
    [('scissors 'paper) 'win]
    [('paper 'rock) 'win]
    [(_ _) 'lose]))

(define (score player opponent)
  (+ (match player
       ['rock 1]
       ['paper 2]
       ['scissors 3])
     (match (outcome player opponent)
       ['win 6]
       ['draw 3]
       ['lose 0])))

(module+ part1
  (call-with-input-file "input"
    (lambda (in)
      (for/sum ([round (parse-input in)])
        (define opponent (interp-choice (first round)))
        (define player (interp-choice (second round)))
        (score player opponent)))))

;; ---------- PART 2 ----------

(define (interp-result char)
  (match char
    [#\X 'rock]
    [#\Y 'paper]
    [#\Z 'scissors]))

(module+ part2
  (call-with-input-file "input"
    (lambda (in)
      (for/sum ([round (parse-input in)])
        (define opponent (interp-choice (first round)))
        (define result (interp-result [second round]))
        (define player (findf (lambda (p)
                                (eq? (outcome p opponent) result))
                              *choices*))
        (score player opponent)))))
