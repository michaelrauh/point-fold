#lang racket

(provide corpus->tree (struct-out node) subtree-at-path children-names node-at-name child-intersect diagonal-prune decode vocab-size make-span-hash make-depth-hash)
(struct node (name children))
(require racket/trace)

(define (make-span-hash tree)
  (make-hash (map cons (hash-keys (node-children tree)) (map (λ (n) (length (hash-keys (node-children n)))) (hash-values (node-children tree))))))

(define (diagonal-prune names diagonal)
  (filter (λ (n) (not (member n diagonal =))) names))

(define (make-depth-hash tree)
  (make-hash (map cons (hash-keys (node-children tree)) (map get-depth (hash-values (node-children tree))))))

(define (get-depth tree)
  (cond
   [(empty? (hash-keys (node-children tree))) 0]
    [else (add1 (apply max (map get-depth (hash-values (node-children tree)))))]))

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

(define (subtree-at-path depth-hash span-hash depth span tree path)
  (cond
    [[false? tree] #f]
    [[empty? path] (begin
                     (define names (hash-keys (node-children tree)))
                     (define depth-filtered (filter (λ (n) (<= depth (hash-ref depth-hash n))) names))
                     (filter (λ (n) (<= span (hash-ref span-hash n))) depth-filtered))]
    [else (subtree-at-path depth-hash span-hash depth span
     (node-at-name span tree (car path))
     (cdr path))]))

(define (node-at-name span tree name)
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
   (map string-downcase (map string-trim (string-split corpus #px"\\!|\\.|\\?|\\;|\\:")))))

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
                    updated-children))
            (let ([updated-children (hash-set (node-children tree) word-to-add (add-phrase encoder (cdr phrase) (make-leaf word-to-add)))])
              (node (node-name tree)
                    updated-children))))))

(define (make-leaf name)
  (node name (hash)))

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
  (check-equal? (children-names (corpus->tree "a b. a.")) (list 0 1))
  (check-equal? (subtree-at-path 0 (corpus->tree "a b") null) '(0 1))
  (check-equal? (subtree-at-path 0 (corpus->tree "a b") (list 0)) 0)
  (check-equal? (subtree-at-path 0 (corpus->tree "a b") (list 0 1)) 1)
  (check-equal? (node-at-name (corpus->tree "a b") 0) 0)
  (check-equal? (diagonal-prune (corpus->tree "a b. d e") '(0 3)) '(1 2))
)