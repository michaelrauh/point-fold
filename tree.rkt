#lang racket

(require threading)
(require math/array)
(require math)
(require racket/vector)
(struct node (name children span depth) #:transparent) ; transparent will make this slow

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
  (remove-duplicates
   (map string-downcase (map string-trim (string-split corpus #px"\\!|\\.|\\?\\;\\:")))))

(define (corpus->sentences corpus)
  (filter cons? (map string-split (corpus->sentence-strings corpus))))

(define (corpus->tree corpus)
  (for/fold ([acc (make-tree)]) ([p (corpus->sentences corpus)])
    (add-phrase-to-tree p acc)))

;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (search corpus dims)
  (define template (dims->template dims))
  (define tree (corpus->tree corpus))
  (fold template tree))

; folds according to template while searching tree. Returns either a list of names or #f
(define (fold template tree)
  (advance tree template null))

; may return false
; otherwise returns the whole node
(define (subtree-at-path tree path)
  1)

(define (span-prune tree span)
  1)

(define (depth-prune tree depth)
  1)

(define (child-intersect l)
  1)

(define (diagonal-prune tree diagonal)
  1)

; resolve links in the rules
; get in to the tree at paths, prune, and return candidate words for next step in the results. This may be empty
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

(define (make-example dims)
  1) ; use indexes-array

(define (dims->spans dims)
  (pack-arr (dims->span-arr dims)))

(define (dims->depths dims)
  1)

(define (dims->paths dims)
  1)

(define (dims->diagonals dims)
  1)

(define (transpose xss)
  (apply map list xss))

; todo make tests with these as setups to drive out accesors to trees. If the trees are not well formed the accessors will fail

;(add-phrase (list "a" "b" "c" "d") (make-tree))
;(add-phrase (list "a" "b" "e" "f") (add-phrase (list "a" "b" "c" "d") (make-tree)))
;(add-phrase-to-tree (list "a" "b" "c" "d" "a" "c" "b" "d") (make-tree))
;(corpus->tree "a b. c d! e f? g h a c b d")

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

(module+ test
  (require rackunit)
  (check-true (name-exists? (node-children (add-phrase (list "example") (make-tree))) "example"))
  (check-false (name-exists? (node-children (add-phrase (list "example") (make-tree))) "not example"))
  (check-equal? (suffixes (list 1 2 3)) '((1 2 3) (2 3) (3)))
  (check-equal? (pack-arr (array #[#[0 1 2] #[3 4 5]]))
                '(0 3 1 4 2 5))
  (check-equal? (dims->span-arr '(3 3 3))
                (array #[#[#[3 3 2] #[3 3 2] #[2 2 1]] #[#[3 3 2] #[3 3 2] #[2 2 1]] #[#[2 2 1] #[2 2 1] #[1 1 0]]]))
  (check-equal? (dims->spans '(3 3 3)) '(3 3 3 3 2 3 2 3 3 2 2 2 2 3 2 2 2 1 2 2 1 2 1 1 1 1 0)))
                
                          