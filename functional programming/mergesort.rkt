;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |CS HW 5:2|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
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