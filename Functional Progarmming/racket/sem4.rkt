#lang racket

(define (is-sufix? a b)
  (cond [(= a 0) #t]
        [(= b 0) #f]
        [(= (remainder a 10) (remainder b 10)) (is-sufix? (quotient a 10) (quotient b 10))]
        [else #f]))

(define (substr? a b)
  (cond [(< b a) #f]
        [(is-sufix? a b) #t]
        [else (substr? a (quotient b 10))]))

(define (my-identity x) x)

(define (my-compose f g)
  (λ (x) (f (g x))))

;((my-compose (λ (x) (* x x) ) (λ (x) (- x 4))) 6)

(define (my-negate p?)
  (my-compose not p?))

;((my-negate even?) 13)


(define (my-curry f x)
  (curry f x))
;((my-curry (λ (a b c) (+ a (* b c))) 2) 4 5)

(define (difference F a b)
  (- (F b)  (F a)))

;(difference (λ (x) (* 2 x)) 3 10)

(define f (λ (x) (* 2 x)))
;(f 5)

(define (derive f eps)
  (λ (x) (/ (- (f (+ x eps)) (f x)) eps)))

;((derive (λ (x) (* 2 x x)) 1e-4) 5)
(define (h x) (* 2 x x))

(define (derive-n n f eps)
  (if (= n 0)
      f
      (derive (derive-n (- n 1) f eps) eps)))

(define EMPTY -1)
(define (make-pair a b)
  (λ (x) (if x a b)))

(define (one p)
  (p #t))

(define (two p)
  (p #f))

(define (empty-list? list)
  (and (not (procedure? list)) (= list EMPTY)))

(define list (make-pair 1 (make-pair 2 (make-pair 3 EMPTY))))

(define (count-elements list)
  (if (not (empty-list? list))
      (+ 1 (count-elements (two list)))
      0))

(count-elements list)




