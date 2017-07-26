#lang typed/racket/base

(provide (all-defined-out))

(require "misc.rkt")

(define-enumeration* psd-color-mode #:+> PSD-Color-Mode ; order matters
  color-mode->integer integer->color-mode
  [0 Bitmap Grayscale Indexed RGB CMYK Multichannel Duotone Lab])

(define-enumeration* psd-compression-method #:+> PSD-Compression-Method ; order matters
  compression-method->integer integer->compression-method
  [0 Raw RLE ZIP ZIP/prediction])
