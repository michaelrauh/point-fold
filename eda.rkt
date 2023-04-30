#lang racket

(require math/number-theory)
(define (corpus->sentences corpus)
  (filter cons? (map string-split (corpus->sentence-strings corpus))))

(define (corpus->sentence-strings corpus)
  (remove-duplicates
   (map string-downcase (map string-trim (string-split corpus #px"\\!|\\.|\\?|\\;|\\:")))))

(define (all-preceding sentence)
  (cons (last sentence)
        (filter cons? (map (λ (pos) (drop-right (drop sentence pos) 1)) (range (length sentence))))))

(define (precedings corpus)
  (map all-preceding (filter cons? (apply append (map prefixes (corpus->sentences corpus))))))

(define (make-preceding-map corpus)
  (for/fold ([acc (hash)]) ([word-and-preceding (precedings corpus)])
    (define word (car word-and-preceding))
    (define points-to (cdr word-and-preceding))
    (hash-update acc word (λ (existing) (remove-duplicates (append existing points-to))) (list))))

(define (make-metadata-map preceding-map)
  (make-hash (hash-map preceding-map (λ (k v) (cons k (map length v))))))

(define (make-supermetadata-map
         metadata-map) ; map from word to count hash table showing how many times that word has appeared in a given position in a phrase
  (make-hash (hash-map metadata-map
                       (λ (k v)
                         (cons k
                               (for/fold ([acc (hash)]) ([num v])
                                 (hash-update acc num add1 0)))))))

(define (prefixes l)
  (map (λ (pos) (take l pos)) (range (add1 (length l)))))

(define (supermeta-example)
  (make-supermetadata-map (make-metadata-map (make-preceding-map (file->string "example.txt")))))

(define (meta-example)
  (make-metadata-map (make-preceding-map (file->string "example.txt"))))

(define (example)
  (make-preceding-map (file->string "example.txt")))

; next thought
; all combinations of the metadata map describe the n-tuple positions that a given key may appear in. K in N choose K is dimensionality. 
; for "the" this is a very, very large number. Naively, it is best to choose the biggest shape,
; then to see if there are enough terms to fill every position in that shape
; after that, a local search can be run by scoring each box filled in here based upon existing phrases and the diagonal rule
; however, since this number is so very large, metasearch may be better to start, then see what is eligible for each position

; as a second look, seeing what is filtered out when asking what can go in the bottom right 2 2 3 position as an example shows a 94% hit rate, so this saves very little too

(define m (meta-example))
;(define t (hash-ref m "the"))
;(define n (length t))
;(for/sum ([k (range n)]) (binomial n k))
    
(define winners (for/list
    ([k (hash-keys m)]
     #:when (subset? (list 2 2 3) (hash-ref m k)))
  k))
(length winners)
(length (hash-keys m))