#lang typed/racket/base

(provide (all-defined-out) ~size)

(require "psd.rkt")
(require "draw.rkt")
(require "parser.rkt")
(require "packbits.rkt")
(require "unsafe/bitmap.rkt")

(define psd-image-data->bitmap : (-> Symbol Bytes PSD-Color-Mode Positive-Index Positive-Index Positive-Byte Positive-Byte
                                     PSD-Compression-Mode Positive-Real (Instance Bitmap%))
  (lambda [func planar-data color-mode width height channels depth compression density]
    (unless (eq? color-mode 'RGB) (throw-unsupported-error func "unimplemeneted color mode: ~a" color-mode))
    (unless (fx= depth 8) (throw-unsupported-error func "unimplemeneted depth : ~a-bpc" depth))
    (unless (or (fx= channels 3) (fx= channels 4)) (throw-unsupported-error func "unimplemented channel count: ~a" channels))
    (case compression
      [(Raw) (planar-data->bitmap planar-data width height channels density)]
      [(RLE) (let* ([scan-lines (fx* height channels)]
                    [intervals (nbytes-pairs (parse-nsizes-list planar-data scan-lines 2 0) (fx* scan-lines 2))])
               (planar-data->bitmap (for/list : (Listof Bytes) ([interval (in-list intervals)])
                                      (unpackbits width planar-data (car interval) (cdr interval)))
                                    width height channels density))]
      [else (throw-unsupported-error func "unimplemented compression method: ~a" compression)])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define throw-unsupported-error : (-> Symbol String Any * Nothing)
  (lambda [func fmt . args]
    (raise (make-exn:fail:unsupported (apply format (string-append "~a: " fmt) func args)
                                      (continuation-marks #false)))))
