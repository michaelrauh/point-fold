#lang racket

(require "metasearch.rkt")
(time (run))

; eliminate pruning <- eliminating depth pruning helps
; make subtree at path faster (obvious approach is to use hashmaps instead of lists)
; combine pruning and finding subtree at path into one
; use a tally vector instead of a set intersect over lists
; dememoize running rules <- this helps slightly

; baseline cpu time: 8934 real time: 8765 gc time: 1273
; no depth pruning cpu time: 8085 real time: 7311 gc time: 1250
; no span pruning cpu time: 146520 real time: 146513 gc time: 31901
; no rules memoization: cpu time: 9145 real time: 8752 gc time: 1458
