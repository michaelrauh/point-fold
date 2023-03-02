#lang racket

(require math/array)
(require math)

(provide dims->template
         pretty-print
         (struct-out rules))
(require racket/trace)

(struct rules (span depth paths diagonal))

(define (pretty-print dims results)
  (define lookup (make-unravel-corr dims))
  (if (false? results)
      results
  (array-map (λ (index) (list-ref results (hash-ref lookup index))) (dims->index-arr dims))))
  

(define (dims->template dims)
  (define spans (dims->spans dims))
  (define depths (dims->depths dims))
  (define paths (dims->paths dims))
  (define diagonals (dims->diagonals dims))
  (map (λ (x) (apply rules x))
       (transpose (list spans depths paths diagonals))))

(define (dims->spans dims)
  (pack-arr (dims->span-arr dims)))

(define (dims->depths dims)
  (define indices (dims->index-arr dims))
  (define depths (array-map (λ (is) (subtract-each (vector->list is) dims)) indices))
  (pack-arr depths))

(define (dims->paths dims)
  (pack-arr (translate-arr (create-paths-matrix dims))))

(define (dims->diagonals dims)
  (define over (pack-arr (other-translate-arr (make-diagonals dims))))
  (for/list ([indices over] [index (range (length over))])
    (filter (λ (i) (< i index)) indices)))

(define (transpose xss)
  (apply map list xss))

(define (pack-arr arr)
  (define dims (vector->list (array-shape arr)))
  (define unravel-corr (make-unravel-corr dims))
  (define pos-order (get-pos-order dims))
  (for/list ([i pos-order])
    (array-ref arr i)))

(define (dims->span-arr dims)
  (define arr (dims->index-arr dims))
  (array-map (λ (i) (point-span i dims)) arr))

(define (dims->index-arr dims)
  (indexes-array (list->vector dims)))

(define (subtract-each index dims)
  (apply max (map (λ (i x) (- (sub1 x) i)) index dims)))

(define (translate-arr paths)
  (array-map
   (λ (point)
     (map (λ (axis) (map (λ (location) (translate location (vector->list (array-shape paths)))) axis))
          point))
   paths))

(define (other-translate-arr paths)
  (define dims (vector->list (array-shape paths)))
  (array-map (λ (point) (map (λ (axis) (translate (vector->list axis) dims)) point)) paths))

(define (create-paths-matrix dims)
  (array-map (λ (point)
               (map (λ (a)
                      (map (λ (x) (list-update (vector->list point) a (λ (_) x)))
                           (range (list-ref (vector->list point) a))))
                    (range (length (vector->list point)))))
             (indexes-array (list->vector dims))))

(define (make-diagonals dims)
  (define grouped (make-group dims))
  (array-map (λ (point) (find-friends point grouped)) (indexes-array (list->vector dims))))

(define (make-group dims)
  (define grouped
    (group-by (λ (x) (sum (vector->list x))) (array->list (indexes-array (list->vector dims)))))
  grouped)

(define (make-unravel-corr dims)
  (define arr (dims->index-arr dims))
  (define pos-order (sort (array->list arr) sum-then-each<))
  (define flatten-hash (make-hash (map cons pos-order (range (length pos-order)))))
  flatten-hash)

(define (get-pos-order dims)
  (define arr (dims->index-arr dims))
  (define pos-order (sort (array->list arr) sum-then-each<))
  pos-order)

(define (point-span index dims)
  (define is (vector->list index))
  (sum (map (λ (i j) (if (eq? i (sub1 j)) 0 1)) is dims)))

(define (translate l dims)
  (define arr (dims->index-arr dims))
  (define pos-order (sort (array->list arr) sum-then-each<))
  (define flatten-hash (make-hash (map cons pos-order (range (length pos-order)))))
  (hash-ref flatten-hash (list->vector l)))

(define (find-friends point grouped)
  (remove point (findf (λ (l) (member point l)) grouped)))

(define (sum-then-each< vec1 vec2)
  (define l1 (vector->list vec1))
  (define l2 (vector->list vec2))
  (cond
    [(< (sum l1) (sum l2)) #t]
    [(< (sum l2) (sum l1)) #f]
    [else (vec< (reverse l1) (reverse l2))]))

(define (vec< l1 l2)
  (cond
    [(< (car l1) (car l2)) #t]
    [(< (car l2) (car l1)) #f]
    [else (vec< (cdr l1) (cdr l2))]))

(module+ test
  (require rackunit)
  (check-equal? (dims->spans '(2 3 4)) '(3 2 3 3 2 2 2 3 3 1 2 2 2 3 2 1 2 2 1 2 1 1 1 0))
  (check-equal? (dims->depths '(2 3 4)) '(3 3 3 2 3 3 2 2 2 3 2 2 2 1 2 2 1 1 2 1 1 1 1 0))
  (check-equal? (dims->paths '(2 2)) '((() ()) ((0) ()) (() (0)) ((2) (1))))
  (check-equal? (dims->diagonals '(3 3)) '(() () (1) () (3) (4 3) () (6) ())))
