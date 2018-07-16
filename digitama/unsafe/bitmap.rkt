#lang typed/racket/base

(require typed/racket/unsafe)

(require "../draw.rkt")

(module unsafe racket/base
  (provide (all-defined-out))
  
  (require racket/draw/unsafe/cairo)
  (require racket/unsafe/ops)

  (require (only-in racket/class send))
  (require (only-in racket/draw make-bitmap))
  (require (only-in racket/math exact-ceiling))
  
  (define (planar-data->bitmap planar-data width height channels density)
    (define-values (bmp surface) (make-argb-image width height density))
    (define pixels (cairo_image_surface_get_data surface))
    (define stride (cairo_image_surface_get_stride surface))
    (when (unsafe-fx= channels 3) (bytes-fill! pixels #xFF))
    (if (bytes? planar-data)
        (case channels
          [(3) (fill-argb-from-rgba! pixels planar-data width height stride fill-rgb!)]
          [(4) (fill-argb-from-rgba! pixels planar-data width height stride fill-argb!)])
        (case channels
          [(3) (fill-argb-from-rgba*! pixels planar-data width height stride channels)]
          [(4) (fill-argb-from-rgba*! pixels planar-data width height stride channels)]))
    (cairo_surface_mark_dirty surface)
    bmp)

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (define-values (A R G B) (if (system-big-endian?) (values 0 1 2 3) (values 3 2 1 0)))

  (define (fill-argb-from-rgba! pixels source width height stride fill-pixel!)
    (define total (unsafe-fx* width height))
    (let fill! ([src-idx 0])
      (when (unsafe-fx< src-idx total)
        (define row (unsafe-fxquotient src-idx width))
        (define col (unsafe-fxremainder src-idx width))
        (fill-pixel! pixels (unsafe-fx+ (unsafe-fx* row stride) (unsafe-fx* col 4)) source src-idx total)
        (fill! (unsafe-fx+ src-idx 1)))))

  (define (fill-argb-from-rgba*! pixels sources width height stride channels)
    (let fill! ([rest sources] [row 0] [channel-idx 0])
      (when (unsafe-fx< channel-idx channels)
        (define channel (case channel-idx [(0) R] [(1) G] [(2) B] [else A]))
        (define offset (unsafe-fx* row stride))
        (define src (unsafe-car rest))
        (let subfill! ([col 0])
          (when (unsafe-fx< col width)
            (define dest-idx (unsafe-fx+ offset (unsafe-fx* col 4)))
            (unsafe-bytes-set! pixels (unsafe-fx+ dest-idx channel) (unsafe-bytes-ref src col))
            (subfill! (unsafe-fx+ col 1))))
        (if (unsafe-fx= (unsafe-fx+ row 1) height)
            (fill! (unsafe-cdr rest) 0 (unsafe-fx+ channel-idx 1))
            (fill! (unsafe-cdr rest) (unsafe-fx+ row 1) channel-idx)))))
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (define (~size size density) (exact-ceiling (/ size density)))
  
  (define (make-argb-image width height density)
    (define img (make-bitmap (~size width density) (~size height density) #true #:backing-scale density))
    (define surface (send img get-handle))
    (when (not surface)
      (raise-arguments-error 'planar-data->bitmap "image is too big"
                             "width" width "height" height
                             "density" density))
    (values img surface))

  (define (fill-rgb! pixels dest-idx source src-idx total)
    (unsafe-bytes-set! pixels (unsafe-fx+ dest-idx R) (unsafe-bytes-ref source (unsafe-fx+ src-idx (unsafe-fx* total 0))))
    (unsafe-bytes-set! pixels (unsafe-fx+ dest-idx G) (unsafe-bytes-ref source (unsafe-fx+ src-idx (unsafe-fx* total 1))))
    (unsafe-bytes-set! pixels (unsafe-fx+ dest-idx B) (unsafe-bytes-ref source (unsafe-fx+ src-idx (unsafe-fx* total 2)))))
  
  (define (fill-argb! pixels dest-idx source src-idx total)
    (fill-rgb! pixels dest-idx source src-idx total)
    (unsafe-bytes-set! pixels (unsafe-fx+ dest-idx A) (unsafe-bytes-ref source (unsafe-fx+ src-idx (unsafe-fx* total 3))))))

(unsafe-require/typed/provide
 (submod "." unsafe)
 [~size (-> Positive-Index Positive-Real Positive-Index)]
 [planar-data->bitmap (-> (U Bytes (Listof Bytes)) Positive-Fixnum Positive-Fixnum Byte Positive-Real (Instance Bitmap%))])
