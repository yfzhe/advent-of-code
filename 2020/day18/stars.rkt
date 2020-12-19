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

(define expr-parser
  (parser
   (tokens value-tokens op-tokens)
   (start start)
   (end EOF)
   (error
    (lambda (tok-ok? tok-name tok-value)
      (error 'parse-expr "bad input")))

   (precs (left + *))

   (grammar
    (start [() #f]
           [(expr) $1])
    (expr [(NUM) $1]
          [(expr + expr) (list '+ $1 $3)]
          [(expr * expr) (list '* $1 $3)]
          [(OPEN-PAREN expr CLOSE-PAREN) $2]))))

;;; parse-expr : String -> Expr
(define (parse-expr str)
  (define in (open-input-string str))
  (expr-parser (lambda () (expr-lexer in))))

(module+ test
  (check-equal? (parse-expr "1") 1)
  (check-equal? (parse-expr "1 + 2") '(+ 1 2))
  (check-equal? (parse-expr "1 + 2 * 4") '(* (+ 1 2) 4))
  (check-equal? (parse-expr "1 + (2 * 3 + 4) * 5 + 6")
                '(+ (* (+ 1 (+ (* 2 3) 4)) 5) 6)))

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

(module+ star1
  (call-with-input-file "input.txt"
    (lambda (in)
      (for/sum ([line (in-lines in)])
        (read-and-eval line)))))
