#lang racket

(require threading
         megaparsack megaparsack/text
         data/applicative data/monad)

(module+ test
  (require rackunit))

;; Game := id * (listof Round)
;; Round := (list nat nat nat)
;; A Round is a tuple representing cubes in red, green and blue, respectively.

(struct game (id rounds) #:transparent)

;; ---------- PARSE ----------
;; I first wrote the "parser" with regexp and string-split.
;; After finishing the problems of today, I rewrote the parser
;; with megaparsack, trying to be familiar with it.
;; Through the megaparsack one is much slower (>10x) than
;; the regexp one, I left it here for keeping my learning result.

(define color/p
  (do [chars <- (many/p letter/p)] ; less strict here
      (pure (string->symbol (list->string chars)))))

(define round/p
  (do [cubes <- (many/p (do [num <- integer/p]
                            space/p
                            [color <- color/p]
                            (pure (cons color num)))
                        #:sep (string/p ", "))]
      (pure (list (dict-ref cubes 'red 0)
                  (dict-ref cubes 'green 0)
                  (dict-ref cubes 'blue 0)))))

(define game/p
  (do (string/p "Game ")
      [id <- integer/p]
      (string/p ": ")
      [rounds <- (many/p round/p #:sep (string/p "; "))]
      (pure (game id rounds))))

;; parse-game : String -> Game
(define (parse-game line)
  (parse-result! (parse-string game/p line)))

(module+ test
  (define game1
    (parse-game "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green"))
  (define game3
    (parse-game "Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red"))

  (check-equal? game1 (game 1 '((4 0 3) (1 2 6) (0 2 0)))))

;; ---------- PART 1 & 2 ----------
;; minimum-cubes : Game -> Round
(define (minimum-cubes game)
  (for/fold ([acc '(0 0 0)])
            ([round (game-rounds game)])
    (map max acc round)))

(module+ test
  (check-equal? (minimum-cubes game1) '(4 2 6))
  (check-equal? (minimum-cubes game3) '(20 13 6)))

;; possible? : Game -> Boolean
(define (possible? game)
  (~> (minimum-cubes game)
      (andmap < _ '(12 13 14))))

(module+ test
  (check-equal? (possible? game1) #t)
  (check-equal? (possible? game3) #f))

(module+ part1
  (call-with-input-file "input"
    (lambda (in)
      (for/sum ([line (in-lines in)]
                #:do [(define game (parse-game line))]
                #:when (possible? game))
        (game-id game)))))

(module+ part2
  (call-with-input-file "input"
    (lambda (in)
      (for/sum ([line (in-lines in)]
                #:do [(define game (parse-game line))])
        (~> (minimum-cubes game)
            (apply * _))))))
