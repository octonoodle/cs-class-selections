;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |CS HW 3:29|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; Exercise 1

; An AExp (Arithmetic Expression) is one of:
; - Number
; - (cons Operation LoA)

; An LoA (List of Arithmetic Expression) is one of:
; - '()
; - (cons AExp LoA)

; An Operation is one of:
; - '+
; - '*

(define 2+2 '(+ 2 2))
(define 2+3*4 '(+ 2 (* 3 4)))
(define JUST2 '(+ 2))
(define NONUM '(+))

; aexp-temp : AExp -> ?
#;(define (aexp-temp aexp)
    (cond [(number? aexp) ...]
          [(cons? aexp) (... (operation-temp (first aexp))
                             (lo-aexp-temp (rest aexp)))]))

; aexp-list-temp : [List-of AExp] -> ?
#;(define (aexp-list-temp aexp-list)
    (cond [(empty? aexp-list) ...]
          [(cons? aexp-list) (... (aexp-temp (first aexp-list)) (aexp-list-temp (rest aexp-list)))]))

; operation-temp : Operation -> ?
#;(define (operation-temp opr)
    (cond [(symbol=? opr '+) ...]
          [(symbol=? opr '*) ...]))

; aexp-machine : AExp -> Number
; evaluate an Arithmetic Expression
(define (aexp-machine aexp)
  (cond [(number? aexp) aexp]
        [(cons? aexp) ; lo-AExp -> Number
         (foldr (λ (exp sum) ((opr->func (first aexp)) (aexp-machine exp) sum)) (opr-identity (first aexp)) (rest aexp))]))
(check-expect (aexp-machine 2+2) 4)
(check-expect (aexp-machine 2+3*4) 14)
(check-expect (aexp-machine JUST2) 2)
(check-expect (aexp-machine NONUM) 0)

; opr->func : Operation -> [X, X, ... X -> X]
; turn a symbolic operation into the actual function
(define (opr->func opr)
  (cond [(symbol=? opr '+) +]
        [(symbol=? opr '*) *]))
(check-expect ((opr->func '+) 2 4) 6)
(check-expect ((opr->func '*) 3 7) 21)

; opr-identity : Operation -> Number
; the identity corresponding to an operation
(define (opr-identity opr)
  (cond [(symbol=? opr '+) 0]
        [(symbol=? opr '*) 1]))
(check-expect (opr-identity '+) 0)
(check-expect (opr-identity '*) 1)

; Exercise 2
; An AlExp (Algebraic Expression) is one of:
; - Number
; - Symbol (indicating a variable)
; - (cons Operation LoAl)

; An LoAl (List of Algebraic Expression) is one of:
; - '()
; - (cons AlExp LoAl)

; An Assignment is a (list Symbol Number)

(define a+b '(+ a b))
(define a+c*d '(+ a (* c d)))
(define 2a+4 '(+ (* a 2) 4))
(define JUSTa '(+ a))

; alexp-temp : AExp -> ?
#;(define (alexp-temp alexp)
    (cond [(symbol? alexp) ...]
          [(number? alexp) ...]
          [(cons? alexp) (... (operation-temp (first alexp))
                              (lo-alexp-temp (rest alexp)))]))

; alexp-list-temp : [List-of AExp] -> ?
#;(define (alexp-list-temp alexp-list)
    (cond [(empty? alexp-list) ...]
          [(cons? alexp-list) (... (alexp-temp (first alexp-list)) (alexp-list-temp (rest alexp-list)))]))

; unambiguous? : AlExp -> Bool
; are there no numbers in an alexp
(define (unambiguous? alexp)
  (cond [(symbol? alexp) #f]
        [(number? alexp) #t]
        [(cons? alexp) (andmap unambiguous? (rest alexp))]))
(check-expect (unambiguous? 2+2) #t)
(check-expect (unambiguous? 2a+4) #f)
(check-expect (unambiguous? NONUM) #t)
(check-expect (unambiguous? a+c*d) #f)

; alexp=? : AlExp, AlExp -> Bool
; are the alexp = ?
(define (alexp=? alexp1 alexp2)
  (cond [(andmap symbol? (list alexp1 alexp2)) (symbol=? alexp1 alexp2)]
        [(andmap number? (list alexp1 alexp2)) (= alexp1 alexp2)]
        [(andmap cons? (list alexp1 alexp2)) (and (symbol=? (first alexp1) (first alexp2))
                                                  (lo-alexp=? (rest alexp1) (rest alexp2)))]
        [else #f]))
(check-expect (alexp=? a+b a+b) #t)
(check-expect (alexp=? 2a+4 2a+4) #t)
(check-expect (alexp=? a+c*d a+c*d) #t)
(check-expect (alexp=? a+c*d a+b) #f)

; lo-alexp=? [List-of AlExp], [List-of AlExp] -> Bool
; is list of alexp = ?
(define (lo-alexp=? list1 list2)
  (cond [(and (empty? list1) (empty? list2)) #t]
        [(or (empty? list1) (empty? list2)) #f]
        [(and (cons? list1) (cons? list2)) (and (alexp=? (first list1) (first list2))
                                                (lo-alexp=? (rest list1) (rest list2)))]))
(check-expect (lo-alexp=? (list a+b 2a+4 2+2)
                          (list a+b 2a+4 2+2)) #t)
(check-expect (lo-alexp=? (list a+b 2a+4 2+2)
                          '()) #f)
(check-expect (lo-alexp=? '() '(a+b)) #f)
(check-expect (lo-alexp=? (list a+b 2a+4 2+2)
                          (list 2a+4 a+b 2+2)) #f)

; assign-once : Assignment, AlExp -> AlExp
; replace all instances of a variable in an AlExp with the given assignment
(define (assign-once asnmt alexp)
  (cond [(symbol? alexp) (if (symbol=? (first asnmt) alexp)
                             (second asnmt)
                             alexp)]
        [(number? alexp) alexp]
        [(cons? alexp) (cons (first alexp)
                             (map (λ (alexp) (assign-once asnmt alexp)) (rest alexp)))]))
(check-expect (assign-once (list 'a 2) JUSTa) JUST2)
(check-expect (assign-once (list 'a 2) NONUM) NONUM)
(check-expect (assign-once (list 'd 4) a+c*d) '(+ a (* c 4)))
(check-expect (assign-once (list 'a 3) 2a+4) '(+ (* 3 2) 4))
(check-expect (assign-once (list 'a 7) '(+ (* (+ a a) a a) (* a a))) '(+ (* (+ 7 7) 7 7) (* 7 7)))

; assign-lots : [List-of Assignment], AlExp -> AlExp
; replace every variable that is defined with its definition in the AlExp
(define (assign-lots asnmt-list alexp)
  (foldr assign-once alexp asnmt-list))
(check-expect (assign-lots (list (list 'a 2)) JUSTa) JUST2)
(check-expect (assign-lots (list (list 'a 2)) NONUM) NONUM)
(check-expect (assign-lots (list (list 'a 2)
                                 (list 'c 3)
                                 (list 'd 37)) a+c*d) '(+ 2 (* 3 37)))

; Exercise 3

; same-contents? : {List-of Number}, {List-of Number} -> Bool
; do two lists have all of the same elements?
(define (same-contents? list1 list2)
  (cond [(and (empty? list1) (empty? list2)) #t]
        [(or (empty? list1) (empty? list2)) #f]
        [(and (cons? list1) (cons? list2))
         (and (member (first list1) list2)
              (same-contents? (rest list1) (remove (first list1) list2)))]))
(check-expect (same-contents? (list) (list)) #t)
(check-expect (same-contents? (list 1) (list 1)) #t)
(check-expect (same-contents? (list 1 2 3 4) (list 2 4 3 1)) #t)
(check-expect (same-contents? (list 1 2 2) (list 2 1 2)) #t)
(check-expect (same-contents? (list 1 2 3) (list 1 2)) #f)

; aexp-kinda=? : AExp, AExp -> Bool
; do the aexps have the same contents, without regard to order?
(define (aexp-kinda=? aexp1 aexp2)
  (cond [(and (number? aexp1)
              (number? aexp2)) (= aexp1 aexp2)]
        [(and (cons? aexp1)
              (cons? aexp2)) (and (symbol=? (first aexp1) (first aexp2))
                           (same-contents?/aexp (rest aexp1) (rest aexp2)))] ; initially thought could be done with same-contents? but NO
        [else #f]))
(check-expect (aexp-kinda=? 5 5) #t)
(check-expect (aexp-kinda=? '(* 8 8) '(* 8 8)) #t)
(check-expect (aexp-kinda=? '(+ 1 2) '(+ 2 1)) #t)
(check-expect (aexp-kinda=? '(+ 2 (* 3 4) 2) '(+ 2 2 (* 4 3))) #t)

(check-expect (aexp-kinda=? 5 3) #f)
(check-expect (aexp-kinda=? '(+ 1 2) 3) #f)
(check-expect (aexp-kinda=? '(+ 1 2) '(* 2 1)) #f)
(check-expect (aexp-kinda=? '(+ 1 2) '(+ 2 2)) #f)
(check-expect (aexp-kinda=? '(+ 2 (* 3 4) 2) '(+ 2 (* 3 4) (* 4 3))) #f)
(check-expect (aexp-kinda=? '(+ 1 (+ 2 3)) '(+ 1 2 3)) #f)

; same-contents?/aexp : {List-of AExp}, {List-of AExp} -> Bool
; do two lists of aexp have all of the same elements?
(define (same-contents?/aexp list1 list2)
  (cond [(and (empty? list1) (empty? list2)) #t]
        [(or (empty? list1) (empty? list2)) #f]
        [(and (cons? list1) (cons? list2))
         (and (ormap (λ (aexp) (aexp-kinda=? aexp (first list1))) list2) ; member
              (same-contents?/aexp (rest list1)
                                   ;(filter (λ (aexp) (not (alexp=? aexp (first list1)))) list2)))]))
                                   (same-contents-remove (first list1) list2)))])) ; remove
(check-expect (same-contents?/aexp (list 5 '(+ 2 1) '(* 3 4 5))
                                   (list '(+ 2 1) 5 '(* 3 4 5))) #t)
(check-expect (same-contents?/aexp '() '()) #t)
(check-expect (same-contents?/aexp (list '(+ (+ 3 5) 3)) '()) #f)

; same-contents-remove : AlExp, [List-of AlExp] -> [List-of AlExp]
; remove but without order?
(define (same-contents-remove aexp list)
  (cond [(empty? list) list]
        [(cons? list) (if (aexp-kinda=? aexp (first list))
                          (rest list)
                          (cons (first list) (same-contents-remove aexp (rest list))))]))
(check-expect (same-contents-remove 3 '()) '())
(check-expect (same-contents-remove 5 (list 5 3 5)) (list 3 5))
(check-expect (same-contents-remove '(* 3 5) (list '(* 4 (* 5 3)) 3 '(* 5 3) '(* 3 5) '(* 3 5)))  (list '(* 4 (* 5 3)) 3 '(* 3 5) '(* 3 5)))
(check-expect (same-contents-remove '(+ 1 2) (list '(+ 4 3) '(+ 2 1))) (list '(+ 4 3)))
(check-expect (alexp=? (same-contents-remove '(* 3 4) '(+ 2 (* 3 4) 2)) (same-contents-remove '(* 3 4) '(+ 2 2 (* 4 3)))) #t)