;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |CS HW 5:9|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define UNSORT-9 (list 5 0 3 3 8 4 4 2 8))
(define UNSORT-10 (list 2 5 0 3 3 8 4 4 2 8))
(define UNSORT-20 (list 11 18 11 2 5 17 3 3 9 6 10 0 2 11 5 16 10 5 10 10))
(define UNSORT-30 (list 23 25 20 21 22 23 18 9 10 15 15 24 27 18 10 19 7 2 0 24 19 12 19 7 2 18 8 21 2 20))
(define r (λ (x) (build-list x (λ (y) (random (* 2 x)))))) ; for testing reliability
(check-expect (list? (r (random 36))) #t)

; nth-smallest : Number, [non-empty List-of Number] -> Number
; the n-th smallest number in the list
(define (nth-smallest n nlist)
    (cond [(empty? (rest nlist)) (first nlist)]
          [(cons? (rest nlist))
           (local [(define MEDIAN-LIST (map exact-median (split-into-fives nlist)))
                   (define PIVOT (nth-smallest (quotient (length MEDIAN-LIST) 2) MEDIAN-LIST))
                   (define SMALLER-EQUAL-BIGGER #|(list (list) (list) (list))|# (pivot-break PIVOT nlist))
                   (define FIRST-ABOVE-P (+ (length (first SMALLER-EQUAL-BIGGER)) (length (second SMALLER-EQUAL-BIGGER))))
                   (define FIRST-BELOW-P (sub1 (length (first SMALLER-EQUAL-BIGGER))))]
             (cond [(>= FIRST-BELOW-P n) (nth-smallest n (first SMALLER-EQUAL-BIGGER))]
                   [(< FIRST-BELOW-P n FIRST-ABOVE-P) PIVOT]
                   [(<= FIRST-ABOVE-P n) (nth-smallest (- n FIRST-ABOVE-P) (third SMALLER-EQUAL-BIGGER))]))]))
(check-expect (nth-smallest 54 '(1)) 1)
(check-expect (nth-smallest 3 UNSORT-10) 3)
(check-expect (nth-smallest 9 UNSORT-20) 9) 
(check-expect (nth-smallest 7 UNSORT-30) 9)
(check-expect (nth-smallest 9 '(0 1 2 3 5 4 6 7 8 9 10)) 9)
;(list 0 2 2 3 3 4 4 5 8 8)
;(list 0 2 2 3 3 5 5 5 6 9 10 10 10 10 11 11 11 16 17 18)
;(list 0 2 2 2 7 7 8 9 10 10 12 15 15 18 18 18 19 19 19 20 20 21 21 22 23 23 24 24 25 27)

; split-into-fives : [List-of Number] -> [List-of [List-of Number]] 
; return a list of 5-number lists, with a smaller list at the end if the number of elements is not divisible by 5
(define (split-into-fives nlist)
  (cond [(<= (length nlist) 5) (cons nlist '())]
        [(> (length nlist) 5) (cons (list (first nlist) (second nlist) (third nlist) (fourth nlist) (fifth nlist))
                                    (split-into-fives (rest (rest (rest (rest (rest nlist)))))))]))
(check-expect (split-into-fives UNSORT-30) '((23 25 20 21 22) (23 18 9 10 15) (15 24 27 18 10) (19 7 2 0 24) (19 12 19 7 2) (18 8 21 2 20)))
(check-expect (split-into-fives '(1 43 5 6 6 90 3 23 6)) '((1 43 5 6 6) (90 3 23 6)))

; exact-median : [List-of Number] -> Number
; the median of a list. returns average of middle two elements if list has no exact center
(define (exact-median list)
  (local [(define SORTED (mergesort list))]
    (cond [(even? (length list)) (/ (+ (list-ref SORTED (/ (length list) 2))
                                       (list-ref SORTED (sub1 (/ (length list) 2)))) 2)]
          [(odd? (length list)) (list-ref SORTED (floor (/ (length list) 2)))])))
(check-expect (exact-median UNSORT-9) 4)
(check-expect (exact-median UNSORT-10) 3.5)

; start mergesort section -->
(define UNSORTED '(2 34 6 5 31 13 23 4 6 5 9 28))
(define SORTED '(2 4 5 5 6 6 9 13 23 28 31 34))

;; mergesort : [List-of Number] -> [List-of Number]
; sort list using Merge-sort algorithm!
(define (mergesort numlist)
  (build-up (break-down numlist)))
(check-expect (mergesort UNSORTED) SORTED)

; break-down : [List-of Number] -> [List-of [List-of Number]]
; split a list into a (list (list x) (list y) (list z) ...)
; where x y and z are numbers from the original list
(define (break-down numlist)
  (foldr (λ (n rest) (cons (list n) rest)) '() numlist))
(check-expect (break-down UNSORTED)
              (list (list 2) (list 34) (list 6) (list 5) (list 31) (list 13) (list 23) (list 4) (list 6) (list 5) (list 9) (list 28)))


; build-up : [List-of [List-of Number]] -> [List-of Number]
; join every two sorted lists together until there is only one list left
(define (build-up list-list)
  (local [(define (pair-off list-list)
            (cond [(empty? list-list) '()]
                  [(empty? (rest list-list)) list-list]
                  [(cons? (rest list-list)) (cons (join-sorted (first list-list) (second list-list))
                                                  (pair-off (rest (rest list-list))))]))]
    (cond [(empty? list-list) '()]
          [(empty? (rest list-list)) (first list-list)]
          [else (build-up (pair-off list-list))])))
(check-expect (build-up '()) '())
(check-expect (build-up (list SORTED)) SORTED)
(check-expect (build-up (break-down UNSORTED)) SORTED)
(check-expect (build-up (list (list 6) (list 1) (list 2))) (list 1 2 6))
(check-expect (build-up (list (list 1 2))) (list 1 2))

; join-sorted : [List-of Number], [List-of Number] -> [List-of Number]
; join two lists, assuming they are sorted
(define (join-sorted list1 list2)
  (cond [(empty? list1) list2]
        [(empty? list2) list1]
        [(and (cons? list1) (cons? list2)) (if (<= (first list1) (first list2))
                                               (cons (first list1) (join-sorted (rest list1) list2))
                                               (cons (first list2) (join-sorted list1 (rest list2))))]))
(check-expect (join-sorted '(1 3) '(2 4)) '(1 2 3 4))
(check-expect (join-sorted '(4 9 1230) '(4 5 18)) '(4 4 5 9 18 1230))
(check-expect (join-sorted '(1) '(5)) '(1 5))
(check-expect (join-sorted '() '(8)) '(8))
(check-expect (join-sorted '(8) '()) '(8))
; <-- end mergesort section

; pivot-break : Number, [List-of Number] -> (list [List-of Number] [List-of Number])
; returns a 'pair' where the first list is all numbers less than the pivot, the second are all equal, and the last is all greater.
(define (pivot-break pivot nlist)
  (local [; pivot-break/acc : Number, [List-of Number], [List-of Number], [List-of Number] -> (list [List-of Number] [List-of Number] [List-of Number])
          (define (pivot-break/acc pivot nlist below equal above)
            (cond [(empty? nlist) (list below equal above)]
                  [(cons? nlist)
                   (pivot-break/acc pivot (rest nlist)
                                    (if (< (first nlist) pivot)
                                        (cons (first nlist) below)
                                        below)
                                    (if (= (first nlist) pivot)
                                        (cons (first nlist) equal)
                                        equal)
                                    (if (> (first nlist) pivot)
                                        (cons (first nlist) above)
                                        above))]))]
    (pivot-break/acc pivot nlist '() '() '())))
(check-expect (pivot-break 1234 '()) '(() () ()))
(check-expect (pivot-break 1 '(1)) (list '() '(1) '()))
(check-expect (pivot-break 12 UNSORT-30) (list
                                          (list 2 8 2 7 0 2 7 10 10 9)
                                          '(12)
                                          (list 20 21 18 19 19 24 19 18 27 24 15 15 18 23 22 21 20 25 23)))
(check-expect (pivot-break 5 (build-list 10 identity)) '((4 3 2 1 0) (5) (9 8 7 6)))