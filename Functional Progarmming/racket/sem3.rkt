#lang racket

(require racket/trace)



(define (is-prime x)
  (define (helper x iter)
    (if (= iter x)
        #t
        (if (= (remainder x iter) 0 )
            #f
            (helper x (+ iter 1)))))
  (helper x 2))

(define (sum-prime-devisors n)

  (define (helper d sum)
    (cond [(> d n) sum]
          [(and (= 0 (remainder n d)) (is-prime d))
           (helper (+ d 1) (+ sum d))]
          [else (helper (+ d 1) sum)]))
  (helper 2 0))

;(sum-prime-devisors 91)


(define (pow x n)
  (if (= n 0 )
      1
      (* x (pow x (- n 1)))))


(define (count-occurences d n)
  (cond [(< n 10) (if (= n d) 1 0)]
        [(= (remainder n 10) d) (+ 1 (count-occurences d (quotient n 10)))]
        [else (count-occurences d (quotient n 10))]))


(define (ascending? n)
  (cond [(< n 10) #t]
        [(< (remainder n 10) (remainder (quotient n 10) 10))
         #f]
        [else (ascending? (quotient n 10))]))

(define (perfect-number? n)
  (define (helper d sum)
    (cond [(= d n) sum]
          [(= (remainder n d) 0) (helper (+ d 1) (+ sum d))]
          [else (helper (+ d 1) sum)]))
  (= (helper 2 1) n))

(define (calc-sum x n)
(define (helper power)
  (if (> power n)
  0
  (+ (pow x power) (helper (+ power 1)))))
  (helper 0))


(define (filtered-sum p? a b)
  (cond [(> a b) 0]
        [(p? a) (+ a (filtered-sum p? (+ a 1) b))]
        [else (filtered-sum p? (+ a 1) b)]))

(filtered-sum even? 1 10)



; Ctrl + \ === lambda === λ

(define (perfect-number-2? n)
  (= n (filtered-sum (λ (d) (= 0 (remainder n d))) 1 (- n 1))))

(perfect-number-2? 6)
(perfect-number-2? 27)
(perfect-number-2? 28)
(perfect-number-2? 8128)







