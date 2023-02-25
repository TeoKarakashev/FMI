#lang racket

(require racket/trace)

(define (ordered? xs pred)
  (cond     [(empty? xs) #t]
            [(empty? (rest xs)) #t]
            [(pred (first xs) (second xs)) (ordered? (rest xs) pred)]
            [else #f]))

;(ordered? '(1 3 10 100) (lambda (x y) (<= (* x x) y)))

(define (longest-common-prefix xs)
  (cond [(empty? xs) '()]
        [(empty? (rest xs)) xs]
        [(< (first xs) (second xs)) (cons (first xs) (longest-common-prefix (rest xs)))]
        [else (list (first xs))]))

;(longest-common-prefix '(1 2 3 7 2 3 4 5 6 8))

(define (max-ordered-sublist xs)
  (define (helper xs longest)
    (define current (longest-common-prefix xs))
    (cond [(<= (length xs) (length longest)) longest]
          [(> (length current) (length longest)) (helper (rest xs) current)]
          [else (helper (rest xs) longest)]))
  (helper xs '()))

;(max-ordered-sublist '(1 5 1 2 3 4 7 8 9 1 2 3))

(define (flatten xs)
  (cond [(empty? xs) '()]
        [(list? (first xs)) (append (flatten (first xs)) (flatten (rest xs)))]
        [else (cons (first xs) (flatten (rest xs)))]))

;(flatten '((1 2 3) (4 5 6) ((7 8) (9 10 (11 (12))))))
; -> '(1 2 3 4 5 6 7 8 9 10 11 12)

(define (assoc key a-list)
  (cond [(empty? a-list) #f]
        [(equal? key (car (car a-list))) (cdr (car a-list))]
        [else (assoc key (cdr a-list))])
  )

;(assoc 1 '((1 . (1 2 3)) (2 . b) (10 . c) (11 . d)))
;(assoc 20 '((1 . a) (2 . b) (10 . c) (11 . d)))


(define (replace lst dict)
  (define (find key)
    (define value (assoc key dict))
    (if value value key))
  (map find lst))


;(replace '(1 10 3 4 2 11) '((1 . a) (2 . b) (10 . c) (11 . d)))



(define (deep-delete xs)
  (define (helper xs k)
    (cond [(empty? xs) '()]
          [(list? (car xs)) (cons (helper (car xs) (+ k 1)) (helper (cdr xs) k))]
          [(> k (car xs)) (helper (cdr xs) k)]
          [else (cons (car xs) (helper (cdr xs) k))])
    )(helper xs 1))


;(deep-delete '(1 (2 (2 4) 1) 0 (3 (1)))) ; -> (1 (2 (4)) (3 ())))

(define (diagonal matrix)
  (define (helper matRow rowIndex curIndex matrixCopy)
    (cond [(empty? (rest matrixCopy)) (cons (last (last matrixCopy)) '())]
          [(= rowIndex curIndex) (cons (first matRow) (helper (first (rest matrixCopy)) (+ rowIndex 1) 0 (rest matrixCopy)))]
          [else (helper (rest matRow) rowIndex (+ curIndex 1) matrixCopy)])
    )
  ; (trace helper)
  (helper (first matrix) 0 0 matrix))

;(define matrix '((1 2 3 4 5)
; (5 6 7 8 9)
; (9 10 11 12 13)
; (13 14 15 16 17)
; (17 18 19 20 21)))
;(diagonal matrix) ; -> (1 6 11 16 21)




(define (tabulate f)
  (define (helper x y)
  (cond [(> x y) '()]
        [else (cons (cons x (f x)) (helper (+ x 1) y))]))
        (λ (x y) (helper x y)))

;((tabulate sqr) 1 5) ; -> '((1 . 1) (2 . 4) (3 . 9) (4 . 16) (5 . 25))
