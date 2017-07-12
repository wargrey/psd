#lang typed/racket/base

(provide ~size planar-data->bitmap)
(provide Bitmap% prop:convertible convert)

(require (only-in typed/racket/draw Bitmap%))
(require typed/racket/unsafe)

(module unsafe racket/base
  (provide (all-defined-out))
  (provide (all-from-out file/convertible))
  
  (require racket/draw/unsafe/cairo)
  (require racket/unsafe/ops)
  (require file/convertible)

  (require (only-in racket/class send))
  (require (only-in racket/draw make-bitmap))
  (require (only-in racket/math exact-ceiling))
  
  (define (planar-data->bitmap planar-data width height channels density)
    (define-values (bmp surface) (make-image width height density))
    (define src (if (list? planar-data) (apply bytes-append planar-data) planar-data))
    (define total (unsafe-fxquotient (bytes-length src) channels))
    (define pixels (cairo_image_surface_get_data surface))
    (for ([nth (in-range total)])
      (case channels
        [(3) (let ([pos (* nth channels)])
               (for ([off (in-range 3)]) (bytes-set! pixels (+ pos off) (bytes-ref src (+ nth (* off total))))))]
        [(4) (let ([alpha (bytes-ref src (+ nth total total total))]
                   [pos (* nth channels)])
               (bytes-set! pixels (+ pos 0) alpha)
               (for ([off (in-range 3)]) (bytes-set! pixels (+ pos off 1) (bytes-ref src (+ nth (* off total))))))]))
    (cairo_surface_mark_dirty surface)
    bmp)

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (define (~size size density) (exact-ceiling (/ size density)))
  
  (define (make-image width height density)
    (define img (make-bitmap (~size width density) (~size height density) #:backing-scale density))
    (define surface (send img get-handle))
    (when (not surface)
      (raise-arguments-error 'planar-data->bitmap "image is too big"
                             "width" width "height" height
                             "density" density))
    (values img surface)))

(unsafe-require/typed
 (submod "." unsafe)
 [prop:convertible Struct-Type-Property]
 [convert (->* ((Instance Bitmap%) Symbol) (Any) Any)]
 [~size (-> Positive-Index Positive-Real Positive-Index)]
 [planar-data->bitmap (-> (U Bytes (Listof Bytes)) Positive-Fixnum Positive-Fixnum Byte Positive-Real (Instance Bitmap%))])
