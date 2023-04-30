#lang racket

(define (corpus->sentences corpus)
  (filter cons? (map string-split (corpus->sentence-strings corpus))))

(define (corpus->sentence-strings corpus)
  (remove-duplicates
   (map string-downcase (map string-trim (string-split corpus #px"\\!|\\.|\\?|\\;|\\:")))))

(define (all-preceding sentence)
  (cons (last sentence) (filter cons? (map (λ (pos) (drop-right (drop sentence pos) 1)) (range (length sentence))))))

(define (precedings corpus)
  (map all-preceding (filter cons? (apply append (map prefixes (corpus->sentences corpus))))))

(define (make-preceding-map corpus)
  (for/fold ([acc (hash)])
            ([word-and-preceding (precedings corpus)])
    (define word (car word-and-preceding))
    (define points-to (cdr word-and-preceding))
    (hash-update acc word (λ (existing) (remove-duplicates (append existing points-to))) (list))))

(define (make-metadata-map preceding-map)
  (make-hash (hash-map preceding-map (λ (k v) (cons k
                                         (map length v))))))

(define (prefixes l)
  (map (λ (pos) (take l pos)) (range (add1 (length l)))))
                           

(define (meta-example)
  (make-metadata-map (make-preceding-map (file->string "example.txt"))))

(define (example)
  (make-preceding-map (file->string "example.txt")))

; todo count lengths