#lang racket

(require "../../intcode/interp.rkt"
         threading)

(module+ star1
  (call-with-input-file "input.txt"
    (lambda (in)
      (~> (parse-program in)
          make-runner
          (run-until-halt _ '(1))
          last))))

(module+ star2
  (call-with-input-file "input.txt"
    (lambda (in)
      (~> (parse-program in)
          make-runner
          (run-until-halt _ '(2))
          last))))
