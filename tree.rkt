#lang racket

(struct node (name children span depth) #:transparent)

(define (make-tree)
  (make-leaf 'root))

(define (make-leaf name)
  (node name (set) 0 0))

(define (add-phrase phrase tree)
  (if (empty? phrase)
      tree
      (let ([word-to-add (car phrase)])
        (if (name-exists? (node-children tree) word-to-add)
            (let ([updated-children (set-add (remove-by-name (node-children tree) word-to-add)
                                             (add-phrase (cdr phrase)
                                                         (select-node-by-name (node-children tree)
                                                                              word-to-add)))])
              (node (node-name tree)
                    updated-children
                    (node-span tree)
                    (add1 (get-max-depth updated-children))))
            (let ([updated-children (set-add (node-children tree)
                                             (add-phrase (cdr phrase) (make-leaf word-to-add)))])
              (node (node-name tree)
                    updated-children
                    (add1 (node-span tree))
                    (add1 (get-max-depth updated-children))))))))

(define (get-max-depth nodes)
  (apply max (set-map nodes (λ (n) (node-depth n)))))

(define (name-exists? nodes name)
  (not (false? (select-node-by-name nodes name))))

(define (remove-by-name nodes name)
  (for/set ([n (set->stream nodes)] #:when (not (eq? (node-name n) name))) n))

(define (select-node-by-name nodes name)
  (for/first ([n (set->stream nodes)] #:when (eq? (node-name n) name))
    n))

(define (add-phrase-to-tree phrase tree)
  (for/fold ([acc tree]) ([p (suffixes phrase)])
    (add-phrase p acc)))

(define (suffixes l)
  (if (= 1 (length l)) (list l) (cons l (suffixes (cdr l)))))

(define (corpus-to-sentence-strings corpus)
  (remove-duplicates (map string-downcase (map string-trim (string-split corpus #px"\\!|\\.|\\?\\;\\:")))))

(define (corpus-to-sentences corpus)
  (filter cons? (map string-split (corpus-to-sentence-strings corpus))))

(define (corpus-to-tree corpus)
  (for/fold ([acc (make-tree)]) ([p (corpus-to-sentences corpus)])
    (add-phrase-to-tree p acc)))

(define (prune-by-depth tree depth)
  1)

(define (walk-down tree name)
  1)

(define (get-children tree)
  1)

(define (prune-to-depth-by-span tree depth span)
  1)

(define (unravel major-axis-root origin major-axis-length)
  1)

(define (fill-pane potential-top-row potential-left-column major-axis-root minor-axis-root)
  ; this will return #f most of the time, as it will assert columns exist every time it adds a new word, top to bottom left to right with the frame filled
  ; it also has to check the diagonal rule
  1)

(define (build-result full-tree tree-cursor dims cursor result)
  ; design idea - only worry about one axis at a time. Build up with the concept of perpindicularity.
  ; more granularly, eat the dims by decrementing the first dim to zero, and then swallow it and go to the next one
  ; even pruning can be more incremental
  ; there has to be a concept of the current coordinate that is being filled in
  ; the tree cursor is a pre-pruned tree. You never have to go back up recursively, just push down and sometimes fail

  (if (cursor-out-of-bounds? dims cursor)
      result
      (begin
        (define pruned (prune-tree tree-cursor dims cursor))
        (define possibilities (enumerate-possibilities pruned cursor))
        (define good-guesses (filter guess-works possibilities))
        (if (not (empty? good-guesses))
            (begin
            (define new-result (map insert-result good-guesses))
            (build-result full-tree pruned dims (increment cursor) new-result))
            (dead-end)))))

(struct ortho (origin axes tree))
          


; idea - can this be a list eater on dims? Perhaps a tail call passing intermediates and pruned trees would help
(define (search tree dims)
  (define dimensionality (length dims))
  (define major-axis-length (car dims))
  (define minor-axis-length (cadr dims))
  (define major-axis-root (prune-by-depth (prune-to-depth-by-span tree (sub1 major-axis-length) dimensionality) major-axis-length))
  (define minor-axis-root (prune-by-depth (prune-to-depth-by-span tree (sub1 minor-axis-length) dimensionality) minor-axis-length))
  (define origins (get-children major-axis-root))


  (for/or ([origin origins])
        (build-result tree (walk-down major-axis-root origin) (walk-down minor-axis-root origin) dims)))
  

(add-phrase (list "a" "b" "c" "d") (make-tree))
(add-phrase (list "a" "b" "e" "f") (add-phrase (list "a" "b" "c" "d") (make-tree)))
(add-phrase-to-tree (list "a" "b" "c" "d" "a" "c" "b" "d") (make-tree))
(corpus-to-tree "a b. c d! e f? g h a c b d")
(module+ test
  (require rackunit)
  (check-true (name-exists? (node-children (add-phrase (list "example") (make-tree))) "example"))
  (check-false (name-exists? (node-children (add-phrase (list "example") (make-tree))) "not example"))
  (check-equal? (suffixes (list 1 2 3)) '((1 2 3) (2 3) (3))))