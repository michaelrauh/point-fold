#lang racket
(require racket/trace)

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
            (let ([updated-children
                   (set-add (remove-by-name (node-children tree) word-to-add)
                            (add-phrase (cdr phrase)
                                        (select-node-by-name (node-children tree) word-to-add)))])
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
  (for/first ([n (set->stream nodes)]
    #:when (eq? (node-name n) name))
    n))

;(trace add-phrase)
(add-phrase (list "a" "b" "c" "d") (make-tree))
(add-phrase (list "a" "b" "e" "f") (add-phrase (list "a" "b" "c" "d") (make-tree)))
(module+ test
  (require rackunit)
  (check-true (name-exists? (node-children (add-phrase (list "example") (make-tree))) "example"))
  (check-false (name-exists? (node-children (add-phrase (list "example") (make-tree)))
                             "not example")))