#lang racket

(provide corpus->tree (struct-out node) subtree-at-path children-names node-at-name child-intersect span-prune depth-prune diagonal-prune)
(struct node (name children span depth) #:transparent)
(require racket/trace)

(define (span-prune tree span)
  (node (node-name tree)
        (filter (λ (n) (> span (node-span n))) (node-children tree))
        (node-span tree)
        (node-depth tree)))

(define (depth-prune tree depth)
  (node (node-name tree)
        (filter (λ (n) (> depth (node-depth n))) (node-children tree))
        (node-span tree)
        (node-depth tree)))

(define (diagonal-prune tree diagonal)
  (node (node-name tree)
        (filter (λ (n) (not (empty? (set-intersect (list->set diagonal) (list->set n)))))
                (node-children tree))
        (node-span tree)
        (node-depth tree)))

(define (child-intersect trees)
  (set->list (set-intersect (map children-names trees))))

(define (children-names tree)
  (set-map (node-children tree) node-name))

(define (subtree-at-path tree path)
  (cond
    [[empty? path] tree]
    [[not (set-member? (children-names tree) (car path))] #f]
    [subtree-at-path
     (node-at-name tree (car path))
     (cdr path)]))

(define (node-at-name tree name)
  (for/first ([c (node-children tree)] #:when (eq? name (node-name c)))
    c))

(define (remove-by-name nodes name)
  (for/set ([n (set->stream nodes)] #:when (not (eq? (node-name n) name))) n))

(define (corpus->tree corpus)
  (for/fold ([acc (make-tree)]) ([p (corpus->sentences corpus)])
    (add-phrase-to-tree p acc)))

(define (corpus->sentences corpus)
  (filter cons? (map string-split (corpus->sentence-strings corpus))))

(define (add-phrase-to-tree phrase tree)
  (for/fold ([acc tree]) ([p (suffixes phrase)])
    (add-phrase p acc)))

(define (make-tree)
  (make-leaf 'root))

(define (corpus->sentence-strings corpus)
  (remove-duplicates
   (map string-downcase (map string-trim (string-split corpus #px"\\!|\\.|\\?\\;\\:")))))

(define (suffixes l)
  (if (= 1 (length l)) (list l) (cons l (suffixes (cdr l)))))

(define (add-phrase phrase tree)
  (if (empty? phrase)
      tree
      (let ([word-to-add (car phrase)])
        (if (name-exists? tree word-to-add)
            (let ([updated-children (set-add (remove-by-name (node-children tree) word-to-add)
                                             (add-phrase (cdr phrase)
                                                         (select-node-by-name-or-default-to-leaf (node-children tree)
                                                                              word-to-add)))])
              (node (node-name tree)
                    updated-children
                    (node-span tree)
                    (add1 (get-max-child-depth updated-children))))
            (let ([updated-children (set-add (node-children tree)
                                             (add-phrase (cdr phrase) (make-leaf word-to-add)))])
              (node (node-name tree)
                    updated-children
                    (add1 (node-span tree))
                    (add1 (get-max-child-depth updated-children))))))))

(define (make-leaf name)
  (node name (set) 0 0))

(define (get-max-child-depth children)
  (apply max (cons 0 (set-map children node-depth))))

(define (name-exists? tree name)
  (member name (children-names tree)))

(define (select-node-by-name tree name)
  (for/first ([n (set->stream tree)] #:when (eq? (node-name n) name))
    n))

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
  (check-equal? (children-names (corpus->tree "a b. a.")) (list "a" "b"))
  )

; subtree-at-path children-names node-at-name child-intersect span-prune depth-prune diagonal-prune