#lang racket

(define (mymin a b) 

    (if(< a b) a b))

(define (inside? x a b)

    (and (<= a x) (<= x b)))


(define (myfunc a b)
    
        (/ (+ (* a a) (* b b)) 2))


(define (myfib n)

    (cond [(= n 0) 0]

          [(= n 1) 1]

          [else (+ (myfib (- n 1)) (myfib (- n 2)))]))


(define (fib-iter n)
  (define (helper prev cur i)
    (if (= i n)
        cur
        (helper cur (+ prev cur) (+ i 1))))
  (helper 1 0 0))

