#lang racket

(require threading)
(require math/array)
(require math)
(require racket/vector)
(require racket/trace)
(require "tree-maker.rkt" "rules-maker.rkt")
(require memo)
(provide search)

(define (search corpus dims)
  (define template (dims->template dims))
  (define tree (corpus->tree corpus))
  (define results (fold template tree))
  results)

(define (fold template tree)
  (advance tree template null))

(define (run-rules tree results current-rule)
  (define concrete-rules (resolve-links current-rule results))
  (define paths (rules-paths concrete-rules))
  ;(define pruned (depth-prune tree (rules-depth concrete-rules)))
  (define dpruned (span-prune tree (rules-span concrete-rules)))
  (define subtrees (map (λ (p) (subtree-at-path dpruned p)) paths))
  (if (member #f subtrees)
      null
      (~> subtrees
          (map (λ (t) (diagonal-prune t (rules-diagonal concrete-rules))) _)
          (child-intersect _))))

(define (resolve-links current-rule results)
  (define active-paths (map.map (λ (x) (list-ref results x)) (rules-paths current-rule)))
  (define active-diagonals (map (λ (x) (list-ref results x)) (rules-diagonal current-rule)))
  (rules (rules-span current-rule) (rules-depth current-rule) active-paths active-diagonals))

(define (map.map pred l)
  (map (λ (x) (map pred x)) l))

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

(module+ test
  (require rackunit)
  (check-equal? (search "a b. c d. a c. b d" '(2 2)) '("a" "b" "c" "d")))