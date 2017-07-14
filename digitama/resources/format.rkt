#lang typed/racket/base

(provide (all-defined-out))
(provide (all-from-out "../resource.rkt"))

(require "../resource.rkt")
(require "../draw.rkt")
(require "../misc.rkt")

(struct: psd-thumbnail : PSD-Thumbnail psd-resource
  ([format : PSD-Thumbnail-Format]
   [width : Positive-Integer]
   [height : Positive-Integer]
   [widthbytes : Positive-Integer]
   [size : Positive-Integer]
   [compressed-size : Positive-Integer]
   [depth : Byte]
   [planes : Byte]
   [image : (Instance Bitmap%)])
  #:transparent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-enumeration* psd-thumbnail-format #:+> PSD-Thumbnail-Format ; order matters
  thumbnail-format->integer integer->thumbnail-format
  [0 kJpegRGB kRawRGB])
