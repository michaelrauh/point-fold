#lang racket

(require "metasearch.rkt")
(time (run))

; make subtree at path faster (obvious approach is to use hashmaps instead of lists)
; consider combining pruning and pathing (just don't follow paths that would be pruned)
; investigate ways to speed up diagonal prune - diagonal prune could be a set subtract if it could return names instead of nodes