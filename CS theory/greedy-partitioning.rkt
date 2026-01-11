#lang racket
(require expect/rackunit)	

;; an Event is a (make-event String Number Number)
; and represents an event with a start and end time
(define-struct event (name start end) #:transparent)

;; a Partitioning is a [List-of [List-of Event]]
; and represents a series of pairwise disjoint collections of events
; where each collection contains no overlapping events
; "overlapping" means there are no two events going on at the same time [start, end)

;; select-maximal: [List-of Event] -> [List-of Event]
;; greedy algorithm to find the maximal length of non-overlapping events
(define (select-maximal event-list)
  (select-maximal-real (sort event-list (λ (event1 event2) (< (event-end event1) (event-end event2))))))

;; select-maximal-real: [List-of Event] -> [List-of Event]
;; accumulator for select-maximal
(define (select-maximal-real event-list)
  (cond [(empty? event-list) '()]
        [(cons? event-list)
         (cons (first event-list)
               (select-maximal-real (list-tail event-list (find-first-unconflict event-list (event-end (first event-list))))))]))

;; find-first-unconflict: [List-of Event], Number -> Number
; return index in given List of the first element with start time after given Number
; an element with start time equal to target time is valid (non-conflicting)
(define (find-first-unconflict list target-time)
  (if (or (empty? list) (>= (event-start (first list)) target-time))
      0
      (add1 (find-first-unconflict (rest list) target-time))))

;selected_activities = []
;  sort(activities, key=activity.finish_time, order=ascending)
;  booked_time_range = [0, 0)
;  for activity in activities:
;    activity_interval = [activity.start_time, activity.finish_time)
;    if no_overlap(activity_interval, booked_time_range):
;      selected_activities.add(activity)
;      booked_time_range = union(booked_time_range, activity_interval)

; aaaaaaa
;    bbbbbb
;       cccc
;          dddd
;
;
; choose a
; find index of first starting after end of a (d)
; (slice list index-of-that)

;; greedy-partition: [List-of Event] -> Partitioning
; partition events into the smallest possible number of partitions
(define (greedy-partition event-list)
  (cond [(empty? event-list) '()]
        [(cons? event-list)
         (local [(define new-partition (select-maximal event-list))]
           (cons new-partition (greedy-partition (remove-subset new-partition event-list))))]))

;; remove-subset: [List-of Event], [List-of Event] -> [List-of Event]
; remove every item in blacklist or return empty
(define (remove-subset blacklist target-list)
  (filter (λ (event) (not (member event blacklist))) target-list))

(define school (list (event "math" 9 10) (event "bio" 9.5 11) (event "cs" 12 15) (event "lunch" 7 8) (event "breakfast" 14 15)))
(define school-maximal (list (event "lunch" 7 8) (event "math" 9 10) (event "cs" 12 15)))
(define school-partitioned (list school-maximal (list (event "bio" 9.5 11) (event "breakfast" 14 15))))
;(sort school (λ (event1 event2) (< (event-end event1) (event-end event2))))
(check-expect (select-maximal '()) '())
(check-expect (select-maximal (list (event "class" 1 2))) (list (event "class" 1 2)))
(check-expect (select-maximal school) school-maximal)
(check-expect (remove-subset '() '()) '())
(check-expect (remove-subset '() (list (event "a" 1 2))) (list (event "a" 1 2)))
(check-expect (remove-subset (list (event "a" 1 2)) '()) '())
(check-expect (remove-subset (list (event "b" 1 2)) (list (event "a" 1 2) (event "b" 1 2) (event "c" 1 2))) (list (event "a" 1 2) (event "c" 1 2)))
(check-expect (greedy-partition '()) '())
(check-expect (greedy-partition (list (event "class" 1 2))) (list (list (event "class" 1 2))))
(check-expect (greedy-partition school) school-partitioned)