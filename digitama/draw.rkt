#lang typed/racket/base

(provide (all-defined-out))
(provide (all-from-out "integer.rkt"))
(provide make-object bitmap% Bitmap%)

(require (only-in typed/racket/draw Bitmap% bitmap%))
(require (only-in racket/class make-object))
(require typed/racket/unsafe)
(require "integer.rkt")

(unsafe-require/typed/provide
 file/convertible
 [prop:convertible Struct-Type-Property]
 [convert (->* ((Instance Bitmap%) Symbol) (Any) Any)])
