#lang racket
(require threading)

(module+ test
  (require rackunit))

;;; Passport = (Listof Field)
;;; Field = (Cons Symbol String)

;;; parse-passport : String -> Passport
(define (parse-passport passport)
  (for/list ([field (in-list (string-split passport))])
    (match-define (list _ key value)
      (regexp-match #rx"(.+):(.+)" field))
    (cons (string->symbol key) value)))

;;; parse-input : Input-Port -> (Listof Passport)
(define (parse-input in)
  (map parse-passport
       (string-split (port->string in) "\n\n")))

(module+ test
  (define in #<<EOF
ecl:gry pid:860033327 eyr:2020 hcl:#fffffd
byr:1937 iyr:2017 cid:147 hgt:183cm

iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884
hcl:#cfa07d byr:1929

hcl:#ae17e1 iyr:2013
eyr:2024
ecl:brn pid:760753108 byr:1931
hgt:179cm

hcl:#cfa07d eyr:2025 pid:166559648
iyr:2011 ecl:brn hgt:59in
EOF
    )
  (define passports (parse-input (open-input-string in)))
  (check-equal? (length passports) 4))

;;; ---------------------- star 1 --------------------------
(define *required-fields*
  '(byr
    iyr
    eyr
    hgt
    hcl
    ecl
    pid))

(define (valid-passport? passport)
  (for/and ([key (in-list *required-fields*)])
    (dict-has-key? passport key)))

(module+ test
  (check-equal? (valid-passport? (first passports)) #t)
  (check-equal? (valid-passport? (second passports)) #f)
  (check-equal? (valid-passport? (third passports)) #t)
  (check-equal? (valid-passport? (fourth passports)) #f))

(module+ star1
  (call-with-input-file "input.txt"
    (lambda~> parse-input
              (count valid-passport? _))))

;;; ---------------------- star 2 --------------------------
(define *rules*
  `((byr . ,(lambda (value)
              (and~> (string->number value)
                     (<= 1920 _ 2002))))
    (iyr . ,(lambda (value)
              (and~> (string->number value)
                     (<= 2010 _ 2020))))
    (eyr . ,(lambda (value)
              (and~> (string->number value)
                     (<= 2020 _ 2030))))
    (hgt . ,(lambda (value)
              (match (regexp-match #px"^([0-9]+)(cm|in)" value)
                [(list _ (app string->number num) "cm")
                 (<= 150 num 193)]
                [(list _ (app string->number num) "in")
                 (<= 59 num 76)]
                [#f #f])))
    (hcl . ,(lambda (value)
              (regexp-match? #px"^#[0-9a-f]{6}$" value)))
    (ecl . ,(lambda (value)
              (~> (string->symbol value)
                  (member _ '(amb blu brn gry grn hzl oth)))))
    (pid . ,(lambda (value)
              (regexp-match? #px"^[0-9]{9}$" value)))))

(define (valid-passport?/2 passport)
  (for/and ([(key pred) (in-dict *rules*)])
    (and~> (dict-ref passport key #f)
           (pred _))))

(module+ test
  (check-equal? (valid-passport?/2 (first passports)) #t))

(module+ star2
  (call-with-input-file "input.txt"
    (lambda~> parse-input
              (count valid-passport?/2 _))))
