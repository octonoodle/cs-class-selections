#lang pl

;;; Exercise I
#|

"5" :
AE
-> PROD (2)
-> ATOM (4)
-> num (5)
-> 5

"(5)" :
AE
-> PROD (2)
-> ATOM (4)
-> ( AE ) (6)
-> ( PROD ) (2)
-> ( ATOM ) (4)
-> ( num ) (5)
-> ( 5 )

"(2 + 5)" :
AE
-> PROD (2)
-> ATOM (4)
-> ( AE ) (6)
-> ( PROD + AE ) (1)
-> ( ATOM + AE ) (4)
-> ( ATOM + PROD ) (2)
-> ( ATOM + ATOM ) (4)
-> ( num + ATOM ) (5)
-> ( num + num ) (5)
-> ( 2 + 5 )

"{{{2 + 5}} * {5 * {{3 + 6}}}}" :
AE
-> PROD (2)
-> ATOM (4)
-> ( AE ) (6)
-> ( PROD ) (2)
-> ( ATOM * PROD ) (3)
-> ( ATOM * ATOM ) (4)
-> ( ( AE ) * ( AE ) ) (6)
-> ( ( PROD ) * ( AE ) ) (2)
-> ( ( ATOM ) * ( AE ) ) (4)
-> ( ( ( AE ) ) * ( AE ) ) (6)
-> ( ( ( AE ) ) * ( PROD ) ) (2)
-> ( ( ( PROD + AE ) ) * ( PROD ) ) (1)
-> ( ( ( PROD + PROD ) ) * ( PROD ) ) (2)
-> ( ( ( ATOM + ATOM ) ) * ( PROD ) ) (4)
-> ( ( ( num + num ) ) * ( PROD ) ) (5)
-> ( ( ( num + num ) ) * ( ATOM * PROD ) ) (3)
-> ( ( ( num + num ) ) * ( num * PROD ) ) (5)
-> ( ( ( num + num ) ) * ( num * ATOM ) ) (4)
-> ( ( ( num + num ) ) * ( num * ( AE ) ) ) (6)
-> ( ( ( num + num ) ) * ( num * ( PROD ) ) ) (2)
-> ( ( ( num + num ) ) * ( num * ( ATOM ) ) ) (4)
-> ( ( ( num + num ) ) * ( num * ( ( AE ) ) ) ) (6)
-> ( ( ( num + num ) ) * ( num * ( ( PROD + AE ) ) ) ) (1)
-> ( ( ( num + num ) ) * ( num * ( ( ATOM + AE ) ) ) ) (4)
-> ( ( ( num + num ) ) * ( num * ( ( num + AE ) ) ) ) (5)
-> ( ( ( num + num ) ) * ( num * ( ( num + PROD ) ) ) ) (2)
-> ( ( ( num + num ) ) * ( num * ( ( num + ATOM ) ) ) ) (4)
-> ( ( ( num + num ) ) * ( num * ( ( num + num ) ) ) ) (5)
-> ( ( (  2  +  5  ) ) * (  5  * ( (  3  +  6  ) ) ) )
|#

;;; Exercise II

(define-type AE
  [Plus Prod AE]
  [Mult Prod])
(define-type Prod
  [Times Atom Prod]
  [Att Atom])
(define-type Atom
  [Num Number]
  [Parens AE])

;;; Exercise III
(: parse-ae : (Sexpr -> AE))
(define (parse-ae sexpr)
  (match sexpr
    [(list left '+ right) (Plus (parse-prod left) (parse-ae right))]
    [thing (Mult (parse-prod thing))]))

(: parse-prod : (Sexpr -> Prod))
(define (parse-prod sexpr)
  (match sexpr
    [(list left '* right) (Times (parse-atom left) (parse-prod right))]
    [thing (Att (parse-atom thing))]))

(: parse-atom : (Sexpr -> Atom))
(define (parse-atom sexpr)
  (match sexpr
    [(list thing) (Parens (parse-ae thing))]
    [(number: num) (Num num)]))

;;; Exercise IV
(: parse : (String -> AE))
(define (parse input)
  (parse-ae (string->sexpr input)))
(test (parse "5") => (Mult (Att (Num 5))))
(test (parse "{5}") => (Mult (Att (Parens (Mult (Att (Num 5)))))))
(test (parse "{2 + 5}") => (Plus (Att (Num 2)) (Mult (Att (Num 5)))))
(test (parse "{{{2 + 5}} * {5 * {{3 + 6}}}}") => (Mult (Times (Parens (Plus (Att (Num 2)) (Mult (Att (Num 5))))) (Times (Num 5) (Att (Parens (Plus (Att (Num 3)) (Mult (Att (Num 6))))))))))

;;; Exercise V

; each constructor corresponds to an instance of one rule of the grammar that was invoked by the parser.
; each time a rule was used, a constructor was made.

;;; Exercise VI

; the double parenthesis around {{2 + 5}} seem unnecessary, but the outer pair serves to indicate
; parenthesis and the inner pair communicates the structure of the (Plus ) constructor.
; if one pair were removed, parse-atom would not treat the expression as parens around a single expression (List {})
; and it would error. 
; (list (list 2 + 5) * thing) 
; parse-ae: (Mult (list (list 2 + 5) * thing)) -> 
; parse-prod: (Mult (Times (list 2 + 5) thing)) ->
; parse-atom: (list 2 + 5) -> error: no pattern matches in parse-atom

;;; Exercise VII

; {{{2 + 5}} * {5 * {{3 * 6}}}}
; {{{2 + 5}} * {5 * {3 * 6}}}
; (Times 5 (list 3 * 6))
; (Times 5 (parse-prod (list 3 * 6)))
; (Times 5 (Times 3 6)) -> ignoring (Num ...) step
; since the parens are on the right side of the times,
; it would be safe to remove them since they will be immediately converted to a times.
; unlike if they were a plus, in which case they would be sent through a parse-atom step.

;; Exercise VIII
; it's kind of ugly, but interesting to know about. i feel like we can
; leave infix operators behind in the realm of pencil and paper math.