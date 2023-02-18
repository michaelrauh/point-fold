#lang racket

(require threading)
(require math/array)
(require math)
(require racket/vector)
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
  (remove-duplicates
   (map string-downcase (map string-trim (string-split corpus #px"\\!|\\.|\\?\\;\\:")))))

(define (corpus->sentences corpus)
  (filter cons? (map string-split (corpus->sentence-strings corpus))))

(define (corpus->tree corpus)
  (for/fold ([acc (make-tree)]) ([p (corpus->sentences corpus)])
    (add-phrase-to-tree p acc)))

(define (subtree-at-path tree path)
  (cond ([empty? path] tree)
        ([not (member (car path) (children-names tree))] #f)
        (subtree-at-path (node-at-name tree (car path)) (cdr path))))

(define (node-at-name tree name)
  (for/first ([c (node-children tree)]
              #:when (eq? name (node-name c)))
    c))

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
        (filter (λ (n) (not (empty? (set-intersect (list->set diagonal) (list->set n))))) (node-children tree))
        (node-span tree)
        (node-depth tree)))

(define (children-names tree)
  (map node-name (node-children tree)))
  
(define (child-intersect trees)
  (set->list (set-intersect (map children-names trees))))

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
  (array-map (λ (point) (map (λ (a) (map (λ (x) (list-update (vector->list point) a (λ (_) x))) (range (list-ref (vector->list point) a)))) (range (length (vector->list point)))))
           (indexes-array (list->vector dims))))

(define (translate l dims)
  (define arr (dims->index-arr dims))
  (define pos-order (sort (array->list arr) sum-then-each<))
  (define flatten-hash (make-hash (map cons (range (length pos-order)) pos-order)))
  (hash-ref flatten-hash l))
  
(define (translate-arr paths)
  (array-map (λ (point) (map (λ (axis) (map (λ (location) (translate location (vector->list (array-shape paths)))) axis)) point)) paths))

(define (dims->paths dims)
  (pack-arr (translate-arr (create-paths-matrix dims))))

(define (find-friends point grouped)
  (remove point (findf (λ (l) (member point l)) grouped)))

(define (make-diagonals dims)
  (define grouped (group-by (λ (x) (sum (vector->list x))) (array->list (indexes-array (list->vector dims)))))
  (array-map (λ (point) (find-friends point grouped)) (indexes-array (list->vector dims))))

(define (dims->diagonals dims)
  (pack-arr (translate-arr (make-diagonals dims))))

(module+ test
  (require rackunit)
  (check-true (name-exists? (node-children (add-phrase (list "example") (make-tree))) "example"))
  (check-false (name-exists? (node-children (add-phrase (list "example") (make-tree))) "not example"))
  (check-equal? (suffixes (list 1 2 3)) '((1 2 3) (2 3) (3)))
  (check-equal? (pack-arr (array #[#[0 1 2] #[3 4 5]]))
                '(0 3 1 4 2 5))
  (check-equal? (dims->span-arr '(3 3 3))
                (array #[#[#[3 3 2] #[3 3 2] #[2 2 1]] #[#[3 3 2] #[3 3 2] #[2 2 1]] #[#[2 2 1] #[2 2 1] #[1 1 0]]]))
  (check-equal? (dims->spans '(3 3 3)) '(3 3 3 3 2 3 2 3 3 2 2 2 2 3 2 2 2 1 2 2 1 2 1 1 1 1 0))
  (check-equal? (dims->depths '(3 3 3 ))
'(2 2 2 2 2 2 2 2 2 2 2 2 2 1 2 2 2 2 1 1 2 1 2 1 1 1 0)))