#lang racket

(require racket/trace)

(define (reverse n result)
  (cond [(= n 0) result]
        [else (reverse (quotient n 10) (+ (* result 10) (remainder n 10)))]))

(define (is-pali n)
  (define reversed (reverse n 0))
  (if (= n reversed) #t #f))

(define (next-pali n)
  (cond [(is-pali (+ n 1)) (+ n 1)]
        [else (next-pali (+ n 1))]))

(define (make-evens-right lst)
  (flatten lst))


(define (alt-split xs)
  (define (helper parity xs odds evens)
    (cond [(empty? xs) (cons odds evens )]
          [(= parity 1) (helper 0 (cdr xs) (flatten (cons odds (car xs))) evens)]
          [else (helper 1 (cdr xs) odds  (cons (flatten (cons evens (car xs)))'()))]))
  (helper 1 xs '() '()))

(define (get-dist-between point point2)
  (+ (abs (- (car point) (car point2))) (abs (- (cdr point) (cdr point2)))))

(define (helper dist point)
  (cond [(= (get-dist-between point '(0 . 0)) dist) point]
        [else (helper dist (cons 0 (+ 1 (cdr point))))]))

(define (get-dist p)
  (define dist (get-dist-between p '(0 . 0)))
  (if (odd? dist) (cons -1 -1) (helper (/ dist 2) '(0 . 0))))


(define (cointains el lst)
        (cond [(empty? lst) #f]
              [(equal? (car lst) el) #t]
              [else (cointains el (cdr lst))]))

(define (intersect xs ys)
  (cond [(empty? xs) '()]
        [(cointains (car xs) ys) (cons (car xs) (intersect (cdr xs) ys))]
        [else (intersect (cdr xs) ys)]))


(define (intersect-foldr xs ys)
  (foldr (lambda (xs ys) (cons (cointains (car xs) ys) '())) xs ys))

(intersect '("pink" "white" "red") '("yellow" "orange" "black"))
;(intersect-foldr '("yellow" "red" "black") '("yellow" "orange" "black"))


