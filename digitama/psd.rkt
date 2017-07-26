#lang typed/racket/base

(provide (all-defined-out))
(provide PSD-Layer-Infobase)

(require "draw.rkt")
(require "resource.rkt")
(require "layer.rkt")
(require "layer/format.rkt")
(require "image.rkt")

(require (for-syntax racket/base))

(struct PSD-Header
  ([channels : Positive-Byte]
   [width : Positive-Index]
   [height : Positive-Index]
   [depth : Positive-Byte]
   [color-mode : PSD-Color-Mode])
  #:transparent)

(struct PSD-Section PSD-Header
  ([color-data : Special-Comment]
   [resources : (U PSD-Image-Resources Special-Comment)]
   [layers : (U Special-Comment (Listof PSD-Layer-Object))]
   [global-mask : (U PSD-Global-Mask Special-Comment False)]
   [tagged-blocks : (U Special-Comment PSD-Layer-Infobase)]
   [compression-mode : PSD-Compression-Mode]
   [image : (U (Instance Bitmap%) Special-Comment)])
  #:transparent #:mutable)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-syntax (psd-ref! stx)
  (syntax-case stx [λ lambda]
    [(_ self field (λ [maybe-val] make-val ...))
     (with-syntax ([psd-field (datum->syntax #'field (string->symbol (format "PSD-~a" (syntax-e #'field))))]
                   [set-psd-field! (datum->syntax #'field (string->symbol (format "set-PSD-~a!" (syntax-e #'field))))])
       #'(let ([maybe-val (psd-unbox (psd-field self))])
           (cond [(not (bytes? maybe-val)) maybe-val]
                 [else (let ([tmp (let () make-val ...)]) (set-psd-field! self tmp) tmp)])))]
    [(_ self field (lambda [maybe-val] make-val ...))
     #'(psd-ref! self field (λ [maybe-val] make-val ...))]))

(define psd-unbox : (All (a) (-> (U a Special-Comment) (U a Bytes)))
  (lambda [v]
    (cond [(not (special-comment? v)) v]
          [else (assert (special-comment-value v) bytes?)])))
