#lang typed/racket/base

(provide (all-defined-out))

(require "draw.rkt")
(require "resource.rkt")
(require "layer.rkt")
(require "misc.rkt")

(require (for-syntax racket/base))

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
   [global-mask : (U PSD-Global-Mask Special-Comment False)]
   [tagged-blocks : Special-Comment]
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-syntax (psd-ref! stx)
  (syntax-case stx [λ lambda]
    [(_ self field (λ [maybe-val] make-val ...))
     (with-syntax ([psd-field (datum->syntax #'field (string->symbol (format "psd-~a" (syntax-e #'field))))]
                   [set-psd-field! (datum->syntax #'field (string->symbol (format "set-psd-~a!" (syntax-e #'field))))])
       #'(let ([maybe-val (psd-unbox (psd-field self))])
           (cond [(not (bytes? maybe-val)) maybe-val]
                 [else (let ([tmp (let () make-val ...)]) (set-psd-field! self tmp) tmp)])))]
    [(_ self field (lambda [maybe-val] make-val ...))
     #'(psd-ref! self field (λ [maybe-val] make-val ...))]))

(define psd-unbox : (All (a) (-> (U a Special-Comment) (U a Bytes)))
  (lambda [v]
    (cond [(not (special-comment? v)) v]
          [else (assert (special-comment-value v) bytes?)])))

(define throw-unsupported-error : (-> Symbol String Any * Nothing)
  (lambda [func fmt . args]
    (raise (make-exn:fail:unsupported (apply format (string-append "~a: " fmt) func args)
                                      (continuation-marks #false)))))
