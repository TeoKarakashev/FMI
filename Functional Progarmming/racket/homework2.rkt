#lang racket

(define (pair-compose fs xs)
  (define functions-length (length fs))
  (define (helper fs xs functions-length y)
    (cond [(= functions-length 0) 0]
          [(= functions-length 1) ((car fs) (car xs) (identity y))]
          [else (+ ((car fs) (car xs) ((car (cdr fs)) (car (cdr xs)) y)) (helper (cdr (cdr fs)) (cdr (cdr xs)) (- functions-length 2) y))])
    )(λ (y) (helper fs xs functions-length y)))

(define fs (list *
                 (λ (x y) (* x x x y))
                 (λ (x y) (+ x 1 y))
                 (λ (x y) (- x (+ 1 y)))
                 (λ (x y) (* x y 2))))
(define xs '(1 2 3 4 5))

;((pair-compose fs xs) 5)

(define (can-drop point lst)
  (cond [(< point (car lst)) (> (- (car lst ) (cdr lst)) point)]
        [else (< (+ (car lst ) (cdr lst)) point)]))

(define (woodcutters xs)
  (define (helper xs left-point count)
    (cond [(empty? (cdr xs)) (+ count 1)]
          [(can-drop left-point (car xs))  (helper (cdr xs) (car (car xs)) (+ count 1))]
          [(can-drop (car (car (cdr xs))) (car xs))  (helper (cdr xs)  (+ (car (car xs)) (cdr (car xs))) (+ count 1))]
          [else (helper (cdr xs) (car (car xs)) count)]))
  (helper (cdr xs) (car (car xs)) 1))

;(woodcutters '((1 . 2) (2 . 1) (5 . 10) (10 . 9) (19 . 1))) ; → 3
;(woodcutters '((1 . 2) (2 . 1) (5 . 10) (10 . 9) (20 . 1))) ; → 4
;(woodcutters '((10 . 4) (15 . 1) (19 . 3) (20 . 1))) ; → 4
;(woodcutters '((1 . 7) (3 . 11) (6 . 12) (7 . 6) (8 . 5) (9 . 11) (15 . 3) (16 . 10) (22 . 2) (23 . 3) (25 . 7) (27 . 3) (34 . 5) (35 . 10) (37 . 3) (39 . 4) (40 . 5) (41 . 1) (44 . 1) (47 . 7) (48 . 11) (50 . 6) (52 . 5) (57 . 2) (58 . 7) (60 . 4) (62 . 1) (67 . 3) (68 . 12) (69 . 8) (70 . 1) (71 . 5) (72 . 5) (73 . 6) (74 . 4) ))