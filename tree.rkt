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

(define (children tree)
  1)

; folds according to template while searching tree. Returns either a vector of names or #f
(define (fold template tree)
  (define results (list))
  (define current-path null)
  (define (go template tree results current-path)
    (if (eq? (length results) (length template))
      results
      (let-values ([(next-result current-path) (advance tree template current-path results)])
      (if (false? next-result)
          #f
          (go template tree results)))))
  (go template tree results current-path))
  

; if results are false, start a new path
; if results are false and the path is terminal, return false as the next result and null for current path
; otherwise, fill in the next result and pass it back up
; returns next result and new current path
(define (advance tree template results current-path)
  1)
        
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