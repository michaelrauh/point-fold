#lang racket

(require threading)
(require math/array)
(require math)
(require racket/vector)
(require "tree-maker.rkt")

(define (search corpus dims)
  (define template (dims->template dims))
  (define tree (corpus->tree corpus))
  (fold template tree))

(define (fold template tree)
  (advance tree template null))

(define (run-rules tree results current-rule)
  (define concrete-rules (resolve-links current-rule))
  (define paths (rules-paths concrete-rules))
  (define subtrees (map (λ (p) (subtree-at-path tree p)) paths))
  (if (member #f subtrees)
      null
      (~> subtrees
          (map (λ (t) (span-prune t (rules-span concrete-rules))) _)
          (map (λ (t) (depth-prune t (rules-depth concrete-rules))) _)
          (map (λ (t) (diagonal-prune t (rules-diagonal concrete-rules))) _)
          (child-intersect _))))

(define (resolve-links current-rule results)
  (define active-paths (map.map (λ (x) (list-ref results x)) (rules-paths current-rule)))
  (define active-diagonals (map (λ (x) (list-ref results x)) (rules-diagonal current-rule)))
  (rules (rules-span current-rule) (rules-depth current-rule) active-paths active-diagonals))

(define (map.map l pred)
  (map (λ (l) (map (λ (x) (pred x)))) l))

(define (advance tree template results)
  (if (eq? (length template) (length results))
      results
      (let ([valid-words-to-fill-in (run-rules tree results (list-ref template (length results)))])
        (if (empty? valid-words-to-fill-in)
            #f
            (let ([rec-res (map (λ (new-word)
                                  (advance tree template (append results (list new-word))))
                                valid-words-to-fill-in)])
              (let ([ress (filter identity rec-res)]) (if (empty? ress) #f (car ress))))))))

(struct rules (span depth paths diagonal))

(define (dims->template dims)
  (define spans (dims->spans dims))
  (define depths (dims->depths dims))
  (define paths (dims->paths dims))
  (define diagonals (dims->diagonals dims))
  (transpose (list spans depths paths diagonals)))

(define (dims->spans dims)
  (pack-arr (dims->span-arr dims)))

(define (subtract-each index dims)
  (apply max (map (λ (i x) (- (sub1 x) i)) index dims)))

(define (dims->depths dims)
  (define indices (dims->index-arr dims))
  (define depths (array-map (λ (is) (subtract-each (vector->list is) dims)) indices))
  (pack-arr depths))

(define (transpose xss)
  (apply map list xss))

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

(define (make-unravel-corr dims)
  (define arr (dims->index-arr dims))
  (define pos-order (sort (array->list arr) sum-then-each<))
  (define flatten-hash (make-hash (map cons pos-order (range (length pos-order)))))
  flatten-hash)

(define (get-pos-order dims)
  (define arr (dims->index-arr dims))
  (define pos-order (sort (array->list arr) sum-then-each<))
  pos-order)

(define (dims->index-arr dims)
  (indexes-array (list->vector dims)))

(define (pack-arr arr)
  (define dims (vector->list (array-shape arr)))
  (define unravel-corr (make-unravel-corr dims))
  (define pos-order (get-pos-order dims))
  (for/list ([i pos-order])
    (array-ref arr i)))

(define (point-span index dims)
  (define is (vector->list index))
  (sum (map (λ (i j) (if (eq? i (sub1 j)) 0 1)) is dims)))

(define (dims->span-arr dims)
  (define arr (dims->index-arr dims))
  (array-map (λ (i) (point-span i dims)) arr))

(define (create-paths-matrix dims)
  (array-map (λ (point)
               (map (λ (a)
                      (map (λ (x) (list-update (vector->list point) a (λ (_) x)))
                           (range (list-ref (vector->list point) a))))
                    (range (length (vector->list point)))))
             (indexes-array (list->vector dims))))

(define (translate l dims)
  (define arr (dims->index-arr dims))
  (define pos-order (sort (array->list arr) sum-then-each<))
  (define flatten-hash (make-hash (map cons (range (length pos-order)) pos-order)))
  (hash-ref flatten-hash l))

(define (translate-arr paths)
  (array-map
   (λ (point)
     (map (λ (axis) (map (λ (location) (translate location (vector->list (array-shape paths)))) axis))
          point))
   paths))

(define (dims->paths dims)
  (pack-arr (translate-arr (create-paths-matrix dims))))

(define (find-friends point grouped)
  (remove point (findf (λ (l) (member point l)) grouped)))

(define (make-diagonals dims)
  (define grouped
    (group-by (λ (x) (sum (vector->list x))) (array->list (indexes-array (list->vector dims)))))
  (array-map (λ (point) (find-friends point grouped)) (indexes-array (list->vector dims))))

(define (dims->diagonals dims)
  (pack-arr (translate-arr (make-diagonals dims))))
