#lang racket

(require racket/trace)




(define (reverse-list list )
  (define (helper list result)
    (if (null? list)
        result
        (helper (cdr list) (cons (car list ) result))
        )
    )(helper list '()))


;(trace helper)

(define (count-elements-list list)
  (if (null? list)
      0
      (+ 1 (count-elements-list (cdr list)))))


(define (contains list n)
  (cond [(null? list) #f]
        [(equal? n (car list)) #t]
        [else (contains (cdr list) n)])
  )

(define (insert-at list pos num)
  (cond [(null? list) (list num)]
        [(= pos 0) (cons num list)]
        [else (cons (car list) (insert-at (cdr list) (- pos 1) num))]))


(define (smallest list)
  (define (helper list n)
    (cond [(null? list) n]
          [(< (car list) n) (helper (cdr list) (car list))]
          [else (helper (cdr list) n)])
    )(helper list 1000000))


(define (my-remove x lst)
  (cond [(null? lst) (list lst)]
        [(equal? (car lst) x) (cdr lst)]
        [else (cons (car lst) (my-remove x (cdr lst)))]))



(define (remove-all x list)
  (cond [(null? list) '()]
        [(equal? (car list) x) (remove-all x (cdr list))]
        [else (cons (car list) (remove-all x (cdr list)))]))



(define (concat list1 list2)
  (if (null? list1)
      list2
      (cons (car list1) (concat (cdr list1) list2))))

;  (concat '(12 3123) '(8123 123))


(define (reverse-l list)
  (define (helper list res)
    (if (null? list)
        res
        (helper (cdr list) (cons (car list) res))))
  (helper list '()))




(define (remove-duplicates subxs xs)
  (define (helper subys ys)
    (cond [(empty? subys) (remove-duplicates subxs ys)]
          [(empty? ys) xs]
          [(= (first subys) (first ys)) (helper (rest subys) (rest ys))]
          [else (cons (first xs) (remove-duplicates subxs (rest xs)))])
    )(trace helper)(helper subxs xs))


;(remove-duplicates '(1 2) '(1 2 1 2 1))   ; -> '(1)
;(remove-duplicates '(2) '(1 2 1 2 1))     ; -> '(1 1 1)
;(remove-duplicates '(2 1 2) '(1 2 1 2 1)) ; -> '(1 1)
;(remove-duplicates '(2 3) '(1 2 1 2 1))   ; -> '(1 2 1 2 1)

(define (sublist-between start end xs)
  (drop (take xs (+ end 1)) start))

;(sublist-between 2 5 '(0 1 2 3 4 5 6 7)) ; -> '(2 3 4 5)
;(sublist-between 2 3 '(0 1 2 3 4 5 6 7)) ; -> '(2 3)

(define (count-occcurrences subxs xs)
  (define (helper subys ys count helper-xs)
    (cond [(empty? subys) (helper subxs (rest helper-xs) (+ count 1) (rest helper-xs))]
          [(empty? ys) count]
          [(= (first subys) (first ys)) (helper (rest subys) (rest ys) count helper-xs)]
          [else (helper subxs (rest helper-xs) count (rest helper-xs))])
    )(trace helper)(helper subxs xs 0 xs))


;(count-occcurrences '(1 2) '(1 2 1 2 1))   ; -> 2
;(count-occcurrences '(2) '(1 2 1 2 1))     ; -> 2
;(count-occcurrences '(2 1 2) '(1 2 1 2 1)) ; -> 1
;(count-occcurrences '(1 2 1) '(1 2 1 2 1)) ; -> 2







