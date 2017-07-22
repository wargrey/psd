#lang typed/racket/base

(provide (all-defined-out))

(require "misc.rkt")

(define-type PSD-Layer-Rectangle (Vector Fixnum Fixnum Index Index))
(define-type PSD-Blending-Range (Vector Byte Byte Byte Byte))
(define-type PSD-Blending-Ranges (Pairof (Pairof PSD-Blending-Range PSD-Blending-Range)
                                         (Listof (Pairof PSD-Blending-Range PSD-Blending-Range))))

(struct PSD-Layer
  ([name : String]
   [rectangle : PSD-Layer-Rectangle]
   [channels : (Listof (Pairof Fixnum Index))]
   [blend : PSD-Blend-Mode]
   [opacity : Byte]
   [base-clipping? : Boolean]
   [flags : (Listof Symbol)]
   [mask : (Option PSD-Layer-Mask)]
   [blending-ranges : PSD-Blending-Ranges])
  #:transparent)

(struct PSD-Layer-Mask
  ([rectangle : PSD-Layer-Rectangle]
   [default-color : Byte]
   [flags : (Listof Symbol)])
  #:transparent)

(struct PSD-Layer-Mask*
  ([parameter : (U Byte Flonum)]
   [real-flags : (Listof Symbol)]
   [real-background : Byte]
   [real-rectangle : PSD-Layer-Rectangle])
  #:transparent)

(struct PSD-Global-Mask
  ([overlay-colorspace : Fixnum]
   [colors : (List Index Index Index Index)]
   [opacity : PSD-Mask-Opacity]
   [kind : PSD-Mask-Kind])
  #:transparent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-enumeration* psd-mask-opacity #:+> PSD-Mask-Opacity
  mask-opacity->integer integer->mask-opacity #:-> Index
  [transparent 1] [opaque 100])

(define-enumeration* psd-mask-kind #:+> PSD-Mask-Kind
  mask-kind->integer integer->mask-kind #:-> Byte
  [selected 0] [protected 1] [layer-specific 128])

(define-enumeration psd-blend-mode : PSD-Blend-Mode
  [pass norm diss dark mul idiv lbrn dkcl lite scrn
        div lddg lgcl over slit hlit vlit llit plit
        hmix diff smud fsub fdiv hue sat colr lum])
