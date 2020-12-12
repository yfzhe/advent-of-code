#lang racket
(require threading)

(module+ test
  (require rackunit))

;;; Inst = (List Op Nat)
;;; parse-input : Input-Port -> (Listof Inst)
(define (parse-input in)
  (for/list ([line (in-lines in)])
    (match-define (list _ op num)
      (regexp-match #px"([NSEWLRF])(\\d+)" line))
    (list (string->symbol op) (string->number num))))

;;; ---------------------- star 1 --------------------------

;;; a ship hold its position and facing direction,
;;; both position and direction are represented by complex number:

(define (direction d)
  (case d [(E) 1] [(W) -1] [(N) +i] [(S) -i]))

;;; run : (Listof Inst) -> (Values Complex Complex)
(define (run insts)
  (for/fold ([pos 0] [face 1])
            ([inst (in-list insts)])
    (match-define (list op num) inst)
    (case op
      [(N S E W)
       (values (+ pos (* (direction op) num))
               face)]
      [(L)
       (values pos
               (* face (expt +i (/ num 90))))]
      [(R)
       (values pos
               (* face (expt -i (/ num 90))))]
      [(F)
       (values (+ pos (* face num))
               face)])))

(module+ test
  (define in (parse-input (open-input-string #<<EOF
F10
N3
F7
R90
F11
EOF
                                             )))
  (let-values ([(pos face) (run in)])
    (check-equal? pos 17-8i)
    (check-equal? face -i)))

;;; final-distance : ... * (Listof Inst) -> Int
(define (final-distance runner insts)
  (define-values (pos _) (runner insts))
  (+ (abs (real-part pos)) (abs (imag-part pos))))

(module+ star1
  (call-with-input-file "input.txt"
    (lambda~> parse-input
              (final-distance run _))))


;;; ---------------------- star 2 --------------------------

;;; run/2 : (Listof Inst) -> (Values Complex Complex)
(define (run/2 insts)
  (for/fold ([ship 0] [waypoint 10+1i])
            ([inst (in-list insts)])
    (match-define (list op num) inst)
    (case op
      [(N S E W)
       (values ship
               (+ waypoint (* (direction op) num)))]
      [(L)
       (values ship
               (* waypoint (expt +i (/ num 90))))]
      [(R)
       (values ship
               (* waypoint (expt -i (/ num 90))))]
      [(F)
       (values (+ ship (* waypoint num))
               waypoint)])))

(module+ test
  (let-values ([(ship waypoint) (run/2 in)])
    (check-equal? ship 214-72i)
    (check-equal? waypoint 4-10i)))

(module+ star2
  (call-with-input-file
    "input.txt"
    (lambda~> parse-input
              (final-distance run/2 _))))
