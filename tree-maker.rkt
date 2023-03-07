#lang racket

(provide corpus->tree (struct-out node) subtree-at-path children-names node-at-name child-intersect span-prune depth-prune diagonal-prune decode vocab-size)
(struct node (name children span depth))
(require racket/trace)

(define (remove-keys hash keys)
  (for/fold ([h hash])
            ([k keys])
    (hash-remove h k)))

(define (span-prune tree span)
  (node (node-name tree)
        (remove-keys (node-children tree) (map node-name (filter (λ (n) (> span (node-span n))) (hash-values (node-children tree)))))
        (node-span tree)
        (node-depth tree)))

(define (depth-prune tree depth)
  (node (node-name tree)
        (filter (λ (n) (<= depth (node-depth n))) (node-children tree))
        (node-span tree)
        (node-depth tree)))

(define (diagonal-prune tree diagonal)
  (filter (λ (n) (not (member n diagonal =))) (hash-keys (node-children tree))))

(define (child-intersect vocab-size trees)
  (define namess trees)
  (define hit (length trees))
  (define tallies (make-vector vocab-size))
  (for ([names namess])
    (for ([name names])
      (vector-set! tallies name (add1 (vector-ref tallies name)))))

  (for/list ([count tallies]
             [pos (range vocab-size)]
             #:when (= count hit))
    pos))  
  

(define (children-names tree)
  (hash-keys (node-children tree)))

(define (subtree-at-path tree path)
  (cond
    [[false? tree] tree]
    [[empty? path] tree]
    [else (subtree-at-path
     (node-at-name tree (car path))
     (cdr path))])) 

(define (node-at-name tree name)
  (hash-ref (node-children tree) name))

(define (remove-by-name nodes name)
  (remove name nodes (λ (x y) (= (node-name y) x))))

(define (corpus->tree corpus)
  (define encoder (corpus->encoder corpus))
  (for/fold ([acc (make-tree)]) ([p (corpus->sentences corpus)])
    (add-phrase-to-tree encoder p acc)))

(define (vocab-size corpus)
  (hash-count (corpus->encoder corpus)))

(define (corpus->encoder corpus)
  (define strings (remove-duplicates (flatten (corpus->sentences corpus))))
  (make-hash (map cons strings (range (length strings)))))

(define (decode corpus k)
  (define strings (remove-duplicates (flatten (corpus->sentences corpus))))
  (define h (make-hash (map cons (range (length strings)) strings)))
  (hash-ref h k))

(define (corpus->sentences corpus)
  (filter cons? (map string-split (corpus->sentence-strings corpus))))

(define (add-phrase-to-tree encoder phrase tree)
  (for/fold ([acc tree]) ([p (suffixes phrase)])
    (add-phrase encoder p acc)))

(define (make-tree)
  (make-leaf 'root))

(define (corpus->sentence-strings corpus)
  (remove-duplicates
   (map string-downcase (map string-trim (string-split corpus #px"\\!|\\.|\\?\\;\\:")))))

(define (suffixes l)
  (if (= 1 (length l)) (list l) (cons l (suffixes (cdr l)))))

(define (add-phrase encoder phrase tree)
  (if (empty? phrase)
      tree
      (let ([word-to-add (hash-ref encoder (car phrase))])
        (if (name-exists? tree word-to-add)
            (let ([updated-children (hash-set (node-children tree) word-to-add (add-phrase encoder (cdr phrase)
                                                         (select-node-by-name-or-default-to-leaf (node-children tree)
                                                                              word-to-add)))])
              (node (node-name tree)
                    updated-children
                    (node-span tree)
                    (add1 (get-max-child-depth updated-children))))
            (let ([updated-children (hash-set (node-children tree) word-to-add (add-phrase encoder (cdr phrase) (make-leaf word-to-add)))])
              (node (node-name tree)
                    updated-children
                    (add1 (node-span tree))
                    (add1 (get-max-child-depth updated-children))))))))

(define (make-leaf name)
  (node name (hash) 0 0))

(define (get-max-child-depth children)
  (apply max (cons 0 (map node-depth (hash-values children)))))

(define (name-exists? tree name)
  (member name (children-names tree)))

(define (select-node-by-name tree name)
  (hash-ref tree name #f))

(define (select-node-by-name-or-default-to-leaf tree name)
  (define node (select-node-by-name tree name))
  (if node
      node
      (make-leaf name)))

(module+ test
  (require rackunit)
  (check-equal? (node-depth (make-tree)) 0)
  (check-equal? (node-depth (corpus->tree "a")) 1)
  (check-equal? (node-depth (corpus->tree "a. b.")) 1)
  (check-equal? (node-depth (corpus->tree "a b. b.")) 2)
  (check-equal? (node-depth (corpus->tree "a b c. a b. a")) 3)
  (check-equal? (node-span (make-tree)) 0)
  (check-equal? (node-span (corpus->tree "a")) 1)
  (check-equal? (node-span (corpus->tree "a. b.")) 2)
  (check-equal? (node-span (corpus->tree "a b. b.")) 2)
  (check-equal? (node-span (corpus->tree "a b c. a b. a")) 3)
  (check-equal? (node-span (corpus->tree "a. b. c.")) 3)
  (check-equal? (node-span (corpus->tree "a b. a c. a d.")) 4)
  (check-equal? (children-names (corpus->tree "a b. a.")) (list 0 1))
  (check-equal? (node-name (subtree-at-path (corpus->tree "a b") null)) 'root)
  (check-equal? (node-name (subtree-at-path (corpus->tree "a b") (list 0))) 0)
  (check-equal? (node-name (subtree-at-path (corpus->tree "a b") (list 0 1))) 1)
  (check-equal? (node-name (node-at-name (corpus->tree "a b") 0)) 0)
;  ;(check-equal? (child-intersect (list (corpus->tree "a b c d e f g. a b. d e") (corpus->tree "a b c d e f g. a c. g h"))) '("a")) come back to. Actually walk down one.
  (check-equal? (children-names (span-prune (corpus->tree "a b. d e") 1)) '(0 2))
  (check-equal? (diagonal-prune (corpus->tree "a b. d e") '(0 3)) '(1 2))
)