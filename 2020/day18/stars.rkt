#lang racket
(require parser-tools/yacc
         parser-tools/lex
         (prefix-in : parser-tools/lex-sre))

(module+ test
  (require rackunit))

;;; INPUT:
;;; <expr> ::= <num>
;;;          | <expr> + <expr>
;;;          | <expr> * <expr>
;;;          | "(" <expr> ")"
;;; AST:
;;; Expr = Number
;;;      | (List '+ Expr Expr)
;;;      | (List '* Expr Expr)

;;; ---------------------- PARSE --------------------------
(define-tokens value-tokens (NUM))
(define-empty-tokens op-tokens (+ * OPEN-PAREN CLOSE-PAREN EOF))

(define expr-lexer
  (lexer
   [(eof) 'EOF]
   [#\space (expr-lexer input-port)]
   ["+" '+]
   ["*" '*]
   ["(" 'OPEN-PAREN]
   [")" 'CLOSE-PAREN]
   [(:+ (:/ "0" "9")) (token-NUM (string->number lexeme))]))

;;; the difference between part 1 and 2 is the precedence of + and *:
;;; - in part 1, the precedence of + and * is the same;
;;; - but in part 2, + has higher precedence than *.
;;; so make a macro to make parsers
(define-syntax-rule (make-expr-parser prec)
  (parser
   (tokens value-tokens op-tokens)
   (start start)
   (end EOF)
   (error
    (lambda (tok-ok? tok-name tok-value)
      (error 'parse-expr "bad input")))

   prec ;; (precs ...)

   (grammar
    (start [() #f]
           [(expr) $1])
    (expr [(NUM) $1]
          [(expr + expr) (list '+ $1 $3)]
          [(expr * expr) (list '* $1 $3)]
          [(OPEN-PAREN expr CLOSE-PAREN) $2]))))

(define expr-parser
  (make-expr-parser (precs (left + *))))
(define expr-parser/2
  (make-expr-parser (precs (left *) (left +))))

;;; parse-expr : String -> Expr
(define (parse-expr str)
  (define in (open-input-string str))
  (expr-parser (lambda () (expr-lexer in))))

(module+ test
  (check-equal? (parse-expr "1") 1)
  (check-equal? (parse-expr "1 + 2") '(+ 1 2))
  (check-equal? (parse-expr "1 * 2 + 4") '(+ (* 1 2) 4))
  (check-equal? (parse-expr "1 + (2 * 3 + 4) * 5 + 6")
                '(+ (* (+ 1 (+ (* 2 3) 4)) 5) 6)))

;;; parse-expr/2 : String -> Expr
(define (parse-expr/2 str)
  (define in (open-input-string str))
  (expr-parser/2 (lambda () (expr-lexer in))))

(module+ test
  (check-equal? (parse-expr/2 "1 * 2 + 4") '(* 1 (+ 2 4)))
  (check-equal? (parse-expr/2 "1 + (2 * 3 + 4) * 5 + 6")
                '(* (+ 1 (* 2 (+ 3 4))) (+ 5 6))))

;;; ---------------------- EVAL  --------------------------

(define (eval-expr expr)
  (match expr
    [(? number?) expr]
    [(list '+ lhs rhs) (+ (eval-expr lhs) (eval-expr rhs))]
    [(list '* lhs rhs) (* (eval-expr lhs) (eval-expr rhs))]))

(module+ test
  (check-equal? (eval-expr 1) 1)
  (check-equal? (eval-expr '(+ 1 2)) 3)
  (check-equal? (eval-expr '(+ (* 1 2) (+ 3 4))) 9))

;;; read-and-eval : String -> Number
(define (read-and-eval str)
  (eval-expr (parse-expr str)))
(define (read-and-eval/2 str)
  (eval-expr (parse-expr/2 str)))

(module+ star1
  (call-with-input-file "input.txt"
    (lambda (in)
      (for/sum ([line (in-lines in)])
        (read-and-eval line)))))

(module+ star2
  (call-with-input-file "input.txt"
    (lambda (in)
      (for/sum ([line (in-lines in)])
        (read-and-eval/2 line)))))
