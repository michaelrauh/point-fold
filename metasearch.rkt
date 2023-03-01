#lang racket
(require "search.rkt")
(require racket/trace)

(define (metasearch corpus)
  (meta-cur corpus '(2 2) 0))

(define (meta-cur corpus dims pos)
  (define result (search corpus dims))
  (displayln (list dims result))
  (define dims-and-pos (find-next-dims dims (not (false? result)) pos))
  (if (false? dims-and-pos)
      #f
      (letrec
        ([next-dims (car dims-and-pos)]
        [next-pos (second dims-and-pos)])
        (meta-cur corpus next-dims next-pos))))

(define (is-base? dims)
  (empty? (filter (λ (x) (not (= 2 x))) dims)))

(define (next-base dims)
  (list (build-list (add1 (length dims)) (λ (_) 2)) 0))

(define (is-saturated? dims pos)
  (if (zero? pos)
      #f
  (= (list-ref dims (sub1 pos)) (list-ref dims pos))))

(define (increment-next-reset-cur dims pos)
  (list (list-update (list-update dims (add1 pos) add1) pos sub1) (add1 pos)))

(define (increment-next dims pos)
  (list (list-update dims (add1 pos) add1) (add1 pos)))

(define (increment dims pos)
  (list (list-update dims pos add1) pos))

(define (is-at-end? dims pos)
  (= (add1 pos) (length dims)))

(define (find-next-dims dims succeeded? pos)
    (cond [(and (is-base? dims) (not succeeded?)) #f]
          [(and (is-at-end? dims pos) (not succeeded?)) (next-base dims)]
          [(and (is-at-end? dims pos) (is-saturated? dims pos)) (next-base dims)]
          [(is-saturated? dims pos) (increment-next dims pos)]
          [(not succeeded?) (increment-next-reset-cur dims pos)]
           [else (increment dims pos)]))

(metasearch "a b c. d e f. h i j. a d h. b e i. c f j.")

; a b c
; d e f
; h i j

(module+ test
  (require rackunit)
  (check-false (find-next-dims '(2 2) #f 0))
  (check-false (find-next-dims '(2 2 2) #f 0))
  (check-equal? (find-next-dims '(2 2) #t 0) '((3 2) 0))
  (check-equal? (find-next-dims '(3 2) #t 0) '((4 2) 0))
  (check-equal? (find-next-dims '(4 2) #t 0) '((5 2) 0))
  (check-equal? (find-next-dims '(4 2) #f 0) '((3 3) 1))
  (check-equal? (find-next-dims '(4 3) #t 1) '((4 4) 1))
  (check-equal? (find-next-dims '(4 4) #t 1) '((2 2 2) 0))
  (check-equal? (find-next-dims '(4 4 2) #t 1) '((4 4 3) 2))
  (check-equal? (find-next-dims '(4 4 3) #f 2) '((2 2 2 2) 0))
  )

(define (run)
  (metasearch (file->string "example.txt")))

(run)