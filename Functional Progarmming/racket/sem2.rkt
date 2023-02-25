#lang racket

(define (mygcd a b)
  (if (= b 0)
      a
      (mygcd b (remainder a b))))



(define (myMaxDevisor x y)
  (if (= y 1)
      1
      (if (= (remainder x y) 0)
          y
          (myMaxDevisor x (- y 1)))))


(define (myMaxDevisor2 x)
  (define (helper d)
    (if(= 0 (remainder x d))
       d
       (helper (- d 1))))
  (helper (- x 1)))



(define (sumOddsInInterval a b)
  (if (> a b)
      0
      (if (odd? a)
          (+ a (sumOddsInInterval (+ a 1) b))
          (sumOddsInInterval (+ a 1) b))))


(define (prime?2 x)
  (if (=  x 1)
      #f
      (= 1 (myMaxDevisor2 x))))


(define (prime? x)
  (define (helper x d)
    (cond [(= x 1) #f]
          [(= x d)  #t]
          [(= 0 (remainder x d)) #f]
          [else (helper x (+ d 1))]
          ))
  (helper x 2))


(define (is-palindrome x)

  (define (helper x y)
    (if (= x 0)
        y
        (helper (quotient x 10) (+ (* y 10) (remainder x 10)))))
  (= x (helper x 0)))


(define (count-palindromes a b)
  (if (> a b)
      0
      (if (is-palindrome a)
          (+ 1 (count-palindromes (+ a 1) b))
          (count-palindromes (+ a 1) b))))

(define (count-devisors n)
  (define (helper n d)
    (if (> d n)
        0
        (if(= 0 (remainder n d))
           (+ 1 (helper n (+ d 1)))
           (helper n (+ d 1)))))
  (helper n 1))




