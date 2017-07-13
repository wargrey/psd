#lang typed/racket/base

(provide (all-defined-out))
(provide (all-from-out racket/fixnum))
(provide Bitmap% prop:convertible convert)

(require (only-in typed/racket/draw Bitmap%))
(require typed/racket/unsafe)
(require racket/fixnum)

(unsafe-require/typed
 file/convertible
 [prop:convertible Struct-Type-Property]
 [convert (->* ((Instance Bitmap%) Symbol) (Any) Any)])

(define positive-byte? : (-> Any Boolean : Positive-Byte) (λ [v] (and (byte? v) (> v 0))))
(define positive-index? : (-> Any Boolean : Positive-Index) (λ [v] (and (index? v) (> v 0))))
