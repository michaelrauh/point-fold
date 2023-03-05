#lang racket

(require "metasearch.rkt")
(require profile-flame-graph)

(profile (run)
         #:svg-path "my-profile.svg"
         #:preview? #t
         #:delay .0001)