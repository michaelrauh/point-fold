#lang racket
(require math/array)

(define (run dims vocabulary reference)
  (for/fold ([stack (list (make-blank dims))])
            ([i (in-naturals)])
        #:break (or (empty? stack) (full? (car stack)))
    (begin
      (define top (car stack))
      (define rest (cdr stack))
      (define all-to-add (map ((curry add-at-next) top) vocabulary))
              (define additions (for/fold ([acc stack])
                        ([to-add all-to-add]
                         #:when (legal? reference to-add))
                (cons to-add stack)))
              (append additions rest))))


    

; this is a nice time to preallocate. Perhaps to preallocate lots of these
(define (make-blank dims)
  (make-array (list->vector dims) #f))

(define (full? current)
  (array-all-and current))

; this would be much faster if only the one cell were touched, either through mutability or structural sharing
; it would also be better if it filled in distance order rather than row-major
(define (add-at-next box word)
  (define l (array->list box))
  (list->array (array-shape box) (list-set l (index-of l #f) word)))

; this would be much faster if only the changed rows/columns were looked at
(define (legal? reference box)
  (for/and ([phrase (map filter-falses (unwrap box))])
    (set-member? reference phrase)))

(define (filter-falses l)
  (filter identity l))

; there is almost certainly an easier way to do this
(define (unwrap box)
  (define all_orthotope_views (map (λ (ax) (array-axis-swap box ax (sub1 (array-dims box)))) (range (array-dims box))))
  (define phrases (map (λ (pos) (array->list* (array-reshape (list-ref all_orthotope_views pos) (list->vector (list (/ (array-size box) (list-ref (array-shape box) pos)) (list-ref (array-shape box) pos)))))) (range (array-dims box))))
  phrases)