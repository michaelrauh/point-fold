#lang racket

(require threading)
(require math/array)
(require math)
(require racket/vector)
(require "tree-maker.rkt" "rules-maker.rkt")

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