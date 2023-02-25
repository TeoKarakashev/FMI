#lang racket
(require racket/trace)

(define (sum-numbers a b)
  (define (helper n)
    (cond [(< n 10) #t]
          [(> (remainder n 10) (/(- (remainder n 100) (remainder n 10)) 10)) #f]
          [else (helper (quotient n 10))]))
  (cond [(> a b) 0 ]
        [(helper a) (+ a (sum-numbers (+ a 1) b))]
        [else (sum-numbers (+ a 1) b)]))


;(sum-numbers 219 225)


(define (contains list n)
  (cond [(null? list) #f]
        [(= (car (car list)) n) #t]
        [else (contains (cdr list) n)]))

;(contains '((1 . 2) (2 . 3) (3 . 4)) 3)

(define (get-distribution n)
  (define numbSquared (* n n))
  (define (helper numbSquared lst)
    (cond [(= numbSquared 0) lst]
          [(contains (remainder numbSquared 10)) #t]
          [else (helper (quotient numbSquared 10) (cons (remainder numbSquared 10) 1))])
    )(helper (* n n) '()))


(define (product-of-digits n)
  (cond [(= n 0) 1]
        [else (* (remainder n 10) (product-of-digits (quotient n 10)))]))

(define (create-list-of-digits-prodcuts n)
  (define product (product-of-digits n))
  (cond [(< n 10) '()]
        [else (cons product (create-list-of-digits-prodcuts product))]))

(define (persistence n)
  (define res (create-list-of-digits-prodcuts n))
  (if (empty? res) (cons (cons n '()) 1) (cons res  (length res))))



;(persistence 39) ;'((27 14 4) . 3) ; 3*9=27, 2*7=14, 1*4=4
;(persistence 126) ;→ '((12 2) . 2) ; 1*2*6=12, 1*2=2
;(persistence 4) ;→ '((4) . 1)
;(persistence 999) ;→ '((729 126 12 2) . 4)

(define (get-missing-length xss)
  (define len (length xss))
  (define (helper xs k)
    (cond [(empty? xs) k]
          [(= k (length (first xs))) (helper xss (+ k 1))]
          [else (helper (rest xs) k)]))
  (define res (helper xss 1))
  (if (= res 1 ) (error "Empty list") res))


;(get-missing-length '((1 2) (4 5 1 1) (1) (5 6 7 8 9))) ;→ 3
;(get-missing-length '(("a", "a", "a") ("a", "a") ("a", "a", "a",
;"a") ("a") ("a", "a", "a", "a", "a", "a"))) ;→ 5
;(get-missing-length '()) ;→ "Empty list"

(define (reverse-num n)
  (define (helper n res)
    (cond [(= n 0) res]
          [else (helper (quotient n 10) (+ (* res 10) (remainder n 10)))]))
  (helper n 0))

;(quotient 1234 10) ;123
;(remainder 1234 10) ; 4

(define (dig-pow n p)
  (define reversed (reverse-num n))
  (define (helper reversed sum p)
    (cond [(= reversed 0) sum]
          [else (helper (quotient reversed 10) (+ sum (expt (remainder reversed 10) p)) (+ p 1))])
    )(define res (helper reversed 0 p))
  (if (= (remainder res n) 0) (/ res n) -1))

;(dig-pow 89 1) ; -> 1 (8¹ + 9² = 89 = 89 * 1)
;(dig-pow 92 1) ; -> -1 (няма k – такова, че 9¹ + 2² = 92 * k)
;(dig-pow 695 2) ; -> 2 (6² + 9³ + 5⁴ = 1390 = 695 * 2)
;(dig-pow 46288 3) ; -> 51 (4³ + 6⁴ + 2⁵ + 8⁶ + 8⁷ = 2360688 = 46288 * 51)


;(split-at '(1 2 3 4 5 6) 3)

(define (shuffle lst)
  (define pos (/ (length lst) 2))
  (define fst (take lst pos))
  (define sec (drop lst pos))
  (define (helper fst sec)
    (cond [(empty? fst ) '()]
          [else (cons (cons (first fst) (cons (first sec) '())) (helper (rest fst) (rest sec)))]))
  (define res (helper fst sec))
  (flatten res))

;(shuffle '(2 5 1 3 4 7)) ; -> '(2 3 5 4 1 7)
;(shuffle '(1 2 3 4 4 3 2 1)) ; -> '(1 4 2 3 3 2 4 1)
;(shuffle '(1 1 2 2)) ; -> '(1 2 1 2)

(define (zero-counter n)
  (define (helper n k)
    (cond [(< n (expt 5 k)) 0]
          [else (+ (floor (/ n (expt 5 k))) (helper n (+ k 1)))])
    )(helper n 1))

(define  (trailing-zeros n)
  (define (helper n pred)
    (if (pred (zero-counter n)) #t #f)
    )(λ (pred) (helper n pred)))




(define (count-bigger lst el)
  (cond [(empty? lst) 0]
        [(> (car lst) el) (+ 1 (count-bigger (cdr lst) el))]
        [else (count-bigger (cdr lst) el)]))

(define (num-bigger-elements lst)
  (define (helper lst elems)
    (cond [(= elems 0) '()]
          [else (cons (flatten (cons (cons (car lst) (count-bigger (cdr lst) (car lst)))'())) (helper (flatten (cons (cdr lst) (cons (car lst) '()))) (- elems 1)))])
         
    )(helper lst (length lst) ))

;(num-bigger-elements '(5 6 3 4)); → '((5 1) (6 0) (3 3) (4 2))
;(num-bigger-elements '(1 1 1)) ;→ '((1 0) (1 0) (1 0))


(define (repeater str) 
(define (helper str count-times glue)
(cond [(= count 0) ""]
      [else (string-append str glue (helper str (- count-times 1) glue))])
)(λ (count-times glue) (helper str count-times glue)))

 ((repeater "I love Racket") 3 " ")
;"I love Racket I love Racket I love Racket"
 ((repeater "Quack") 5 "!")
;"Quack!Quack!Quack!Quack!Quack"

