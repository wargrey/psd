#lang typed/racket/base

(provide (all-defined-out) PSD-Resource)
(provide (struct-out psd-resource))

(require "../resource.rkt")
(require "../draw.rkt")
(require "../misc.rkt")

(struct: psd-version-info : PSD-Version-Info psd-resource ; 1057
  ([version : Index]
   [has-real-merged-data? : Boolean]
   [writer : String]
   [reader : String]
   [file-version : Index]))

(struct: psd-thumbnail : PSD-Thumbnail psd-resource ; 1036
  ([format : PSD-Thumbnail-Format]
   [width : Positive-Fixnum]
   [height : Positive-Fixnum]
   [widthbytes : Positive-Fixnum]
   [size : Positive-Fixnum]
   [compressed-size : Positive-Fixnum]
   [depth : Byte]
   [planes : Index]
   [image : (Instance Bitmap%)]))

(struct: psd-file-info : PSD-File-Info psd-resource ; 1028 1060
  ([raw : Bytes]))

(struct: psd-grid+guides : PSD-Grid+Guides psd-resource ; 1032
  ([version : Fixnum]
   [horizontal : Fixnum]
   [vertical : Fixnum]
   [guides : (Listof (Pairof Fixnum PSD-Guide-Direction))]))

(struct: psd-print-flags : PSD-Print-Flags psd-resource ; 1011
  ([labels : Boolean]
   [crop-marks : Boolean]
   [color-bars : Boolean]
   [registration-marks : Boolean]
   [negative : Boolean]
   [flip : Boolean]
   [interpolate : Boolean]
   [caption : Boolean]
   [print-flags : Boolean]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-enumeration* psd-thumbnail-format #:+> PSD-Thumbnail-Format ; order matters
  thumbnail-format->integer integer->thumbnail-format
  [0 kJpegRGB kRawRGB])

(define-enumeration* psd-guide-direction #:+> PSD-Guide-Direction ; order matters
  vhselect->integer integer->vhselect
  [0 vertical horizontal])
