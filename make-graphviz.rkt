#lang racket

(require profile/sampler)
(require profile/analyzer)
(require profile/render-graphviz)
(require "metasearch.rkt")


(define sampler (create-sampler (current-thread) 0.005))
(run)
(render (analyze-samples (sampler 'get-snapshots)))

