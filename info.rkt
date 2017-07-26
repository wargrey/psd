#lang info

(define collection 'use-pkg-name)
(define pkg-desc "PSD: Read and Write Photoshop Documents")

(define deps '("base" "typed-racket-lib" "typed-racket-more" "draw-lib"))
(define build-deps '("scribble-lib" "racket-doc"))

(define version "1.0")
(define pkg-authors '(wargrey))
(define test-omit-paths 'all)

(define scribblings '(["tamer/psd.scrbl" (main-doc) (parsing-library)]))
