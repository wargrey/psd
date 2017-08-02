#lang typed/racket/base

(provide (all-defined-out))
(provide (all-from-out "base.rkt"))
(provide (all-from-out "layer.rkt"))

(require "base.rkt")
(require "layer.rkt")

(define read-psd* : (-> (U Path-String Input-Port) [#:backing-scale Positive-Real] [#:try-@2x? Boolean]
                        [#:resolve-resource? Boolean] [#:resolve-layer? Boolean] [#:resolve-tagged-block? Boolean] PSD)
  (lambda [/dev/psdin #:backing-scale [density 1.0] #:try-@2x? [try-@2x? #false] #:resolve-resource? [resolve-resource? #true]
                      #:resolve-layer? [resolve-layer? #true] #:resolve-tagged-block? [resolve-tagged-block? #true]]
    (define psd : PSD (read-psd /dev/psdin #:backing-scale density #:try-@2x? try-@2x?))
    (when resolve-resource? (psd-image-resources psd #:resolve? #true))
    (when resolve-layer? (psd-layers psd #:resolve? #true))
    (when resolve-tagged-block? (psd-tagged-blocks psd #:resolve? #true))
    psd))
