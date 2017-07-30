#lang typed/racket/base

(provide (all-defined-out))
(provide PSD-Layer-Infobase)

(require "draw.rkt")
(require "resource.rkt")
(require "layer.rkt")
(require "layer/format.rkt")
(require "image.rkt")

(require (for-syntax racket/base))

(struct PSD-File
  ([name : Symbol]
   [density : Positive-Real])
  #:transparent)

(struct PSD-Header PSD-File
  ([channels : Positive-Byte]
   [width : Positive-Index]
   [height : Positive-Index]
   [depth : Positive-Byte]
   [color-mode : PSD-Color-Mode])
  #:transparent)

(struct PSD-Section PSD-Header
  ([color-data : Bytes]
   [resources : (U PSD-Image-Resources Bytes)]
   [layers : (U (Listof PSD-Layer-Object) Bytes)]
   [global-mask : (U PSD-Global-Layer-Mask Bytes False)]
   [tagged-blocks : (U PSD-Layer-Infobase Bytes)]
   [image : (U (Instance Bitmap%) Bytes)]
   [compression-method : PSD-Compression-Method]
   [special-size : Positive-Byte])
  #:mutable)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-syntax (psd-ref! stx)
  (syntax-case stx [λ lambda]
    [(_ self field (λ [maybe-val] make-val ...))
     (with-syntax ([psd-field (datum->syntax #'field (string->symbol (format "PSD-~a" (syntax-e #'field))))]
                   [set-psd-field! (datum->syntax #'field (string->symbol (format "set-PSD-~a!" (syntax-e #'field))))])
       #'(let ([maybe-val (psd-field self)])
           (cond [(not (bytes? maybe-val)) maybe-val]
                 [else (let ([tmp (let () make-val ...)]) (set-psd-field! self tmp) tmp)])))]
    [(_ self field (lambda [maybe-val] make-val ...))
     #'(psd-ref! self field (λ [maybe-val] make-val ...))]))
