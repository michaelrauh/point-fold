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

(define (corpus->sentence-strings corpus)
  (remove-duplicates (map string-downcase (map string-trim (string-split corpus #px"\\!|\\.|\\?\\;\\:")))))

(define (corpus->sentences corpus)
  (filter cons? (map string-split (corpus->sentence-strings corpus))))

(define (corpus->tree corpus)
  (for/fold ([acc (make-tree)]) ([p (corpus->sentences corpus)])
    (add-phrase-to-tree p acc)))

(struct rules (span depth paths diagonal))

(define (dims->spans dims)
  1)

(define (dims->depths dims)
  1)

(define (dims->paths dims)
  1)

(define (dims->diagonals dims)
  1)

(define (transpose xss)
  (apply map list xss))

(define (search corpus dims)
  (define template (dims->template dims))
  (define tree (corpus->tree corpus))
  (fold template tree))

; folds according to template while searching tree. Returns either a list of names or #f
; rework this - call advance with tree, template, and empty results and it will return either one result or false
(define (fold template tree)
  (advance tree template null))

; may return false
; otherwise returns the whole node
(define (subtree-at-path path)
  1)

(define (span-prune tree span)
  1)

(define (tree-overpruned? tree)
  1)

(define (depth-prune tree)
  1)

(define (find-overlaps tree other)
  1)

(define (children tree)
  1)

; resolve links in the rules
; get in to the tree at paths, prune, and return candidate words for next step in the results
(define (run-rules tree rules)
  1)

; (struct rules (span depth paths diagonal))
; if the result is done, return it
; if the search space is empty, return false
; if there is an available place to put the next result, recurse on all possible results with that next slot filled in
; once the recurse comes back up, filter falses and if it is empty, return false for the whole batch. If there are multiple that come back and are done, return the first one

; search space is a matter of looking at the rules and running them
(define (advance tree template results)
  (if (eq? (length template) (length results))
      results
      (let ([valid-words-to-fill-in (run-rules tree (list-ref template (length results)))])
        (if (empty? valid-words-to-fill-in)
            #f
            (let ([rec-res (map (λ (new-word) (advance tree template (append results (list new-word)))))])
              (let ([ress (filter identity rec-res)])
                (if (empty? ress)
                    #f
                    (car ress))))))))
                        
                  
; produce a list of rules. Each rule applies to the corresponding location in a result vector. Results only contain names.
; a rule governs what can go in the corresponding location by referencing earlier locations in the vector
; the above helpers will end up zipped together to produce the full template
(define (dims->template dims) 
  1)

(add-phrase (list "a" "b" "c" "d") (make-tree))
(add-phrase (list "a" "b" "e" "f") (add-phrase (list "a" "b" "c" "d") (make-tree)))
(add-phrase-to-tree (list "a" "b" "c" "d" "a" "c" "b" "d") (make-tree))
(corpus->tree "a b. c d! e f? g h a c b d")
(module+ test
  (require rackunit)
  (check-true (name-exists? (node-children (add-phrase (list "example") (make-tree))) "example"))
  (check-false (name-exists? (node-children (add-phrase (list "example") (make-tree))) "not example"))
  (check-equal? (suffixes (list 1 2 3)) '((1 2 3) (2 3) (3))))