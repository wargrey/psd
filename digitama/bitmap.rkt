#lang typed/racket/base

(provide (all-defined-out) ~size)

(require "image.rkt")
(require "draw.rkt")
(require "parser.rkt")
(require "exn.rkt")
(require "unsafe/bitmap.rkt")
(require "../village/packbits.rkt")

(define psd-image-data->bitmap : (-> Symbol Bytes PSD-Color-Mode Positive-Index Positive-Index Positive-Byte Positive-Byte
                                     PSD-Compression-Method Positive-Real (Instance Bitmap%))
  (lambda [func planar-data color-mode width height channels depth compression-method density]
    (unless (eq? color-mode 'RGB) (throw-unsupported-error func "unimplemeneted color mode: ~a" color-mode))
    (unless (fx= depth 8) (throw-unsupported-error func "unimplemeneted depth : ~a-bpc" depth))
    (unless (or (fx= channels 3) (fx= channels 4)) (throw-unsupported-error func "unimplemented channel count: ~a" channels))
    (case compression-method
      [(Raw) (planar-data->bitmap planar-data width height channels density)]
      [(RLE) (let* ([scan-lines (fx* height channels)]
                    [intervals (nbytes-pairs (fx* scan-lines 2) (parse-nsizes-list planar-data 0 2 scan-lines))])
               (planar-data->bitmap (for/list : (Listof Bytes) ([interval (in-list intervals)])
                                      (unpackbits width planar-data (car interval) (cdr interval)))
                                    width height channels density))]
      [else (throw-unsupported-error func "unimplemented compression method: ~a" compression-method)])))
