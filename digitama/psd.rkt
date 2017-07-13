#lang typed/racket/base

(provide (all-defined-out))

(require "draw.rkt")
(require "misc.rkt")

(define-type PSD-Image-Resources (Listof (Pairof Natural Any)))

(struct psd-header
  ([version : PSD-Version]
   [channels : Positive-Byte]
   [width : Positive-Index]
   [height : Positive-Index]
   [depth : Positive-Byte]
   [color-mode : PSD-Color-Mode])
  #:transparent)

(struct psd-section psd-header
  ([color-data : Special-Comment]
   [resources : (U PSD-Image-Resources Special-Comment)]
   [layers : Special-Comment]
   [compression-mode : PSD-Compression-Mode]
   [image : (U (Instance Bitmap%) Special-Comment)])
  #:transparent #:mutable)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-enumeration* psd-version #:+> PSD-Version ; order matters
  version->integer integer->version
  [1 PSD PSB])

(define-enumeration* psd-color-mode #:+> PSD-Color-Mode ; order matters
  color-mode->integer integer->color-mode
  [0 Bitmap Grayscale Indexed RGB CMYK Multichannel Duotone Lab])

(define-enumeration* psd-compression-mode #:+> PSD-Compression-Mode ; order matters
  compression-mode->integer integer->compression-mode
  [0 Raw RLE ZIP ZIP/prediction])
