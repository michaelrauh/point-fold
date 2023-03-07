#lang racket

(require "metasearch.rkt")
(time (run))

; remove depth from node
; do span pruning while traversing down
; try removing span from node and calculating on the fly