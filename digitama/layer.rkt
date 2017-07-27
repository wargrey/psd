#lang typed/racket/base

(provide (all-defined-out))

(require "draw.rkt")
(require "image.rkt")
(require "misc.rkt")
(require "layer/format.rkt")

(define-type PSD-Layer-Mask-Parameter (Vector (Option Byte) (Option Flonum) (Option Byte) (Option Flonum)))
(define-type PSD-Layer-Channel (List Fixnum PSD-Compression-Method Fixnum Fixnum))
(define-type PSD-Blending-Range (Vector Byte Byte Byte Byte))
(define-type PSD-Blending-Ranges (Pairof (Pairof PSD-Blending-Range PSD-Blending-Range)
                                         (Listof (Pairof PSD-Blending-Range PSD-Blending-Range))))

(struct PSD-Layer-Header
  ([id : (U Integer Symbol)]
   [name : String]
   [channels : (Listof PSD-Layer-Channel)]
   [has-transparency-data? : Boolean])
  #:transparent)

(struct PSD-Layer-Object PSD-Layer-Header
  ([record : PSD-Layer-Record]
   [infobase : PSD-Layer-Infobase]
   [image : (U (Instance Bitmap%) Bytes)]
   [color-mode : PSD-Color-Mode]
   [density : Positive-Real]))

(struct PSD-Layer PSD-Layer-Object () #:transparent)
(struct PSD-Layer:Folder PSD-Layer-Object () #:transparent)
(struct PSD-Layer:Open PSD-Layer:Folder () #:transparent)
(struct PSD-Layer:Closed PSD-Layer:Folder () #:transparent)
(struct PSD-Layer:Divider PSD-Layer-Object () #:transparent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(struct PSD-Layer-Record
  ([x : Fixnum]
   [y : Fixnum]
   [width : Index]
   [height : Index]
   [blend : PSD-Blend-Mode]
   [opacity : Byte]
   [base-clipping? : Boolean]
   [flags : (Listof Symbol)]
   [mask : (Option PSD-Layer-Mask)]
   [blending-ranges : PSD-Blending-Ranges]
   [name : String])
  #:transparent)

(struct PSD-Layer-Mask
  ([x : Fixnum]
   [y : Fixnum]
   [width : Index]
   [height : Index]
   [default-color : Byte]
   [flags : (Listof Symbol)]
   [parameter : PSD-Layer-Mask-Parameter])
  #:transparent)

(struct PSD-Layer-Real-Mask PSD-Layer-Mask
  ([flags : (Listof Symbol)]
   [background : Byte]
   [x : Fixnum]
   [y : Fixnum]
   [width : Index]
   [height : Index])
  #:transparent)

(struct PSD-Global-Layer-Mask
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
  [selected 0] [protected 1] [shadowed 128])
