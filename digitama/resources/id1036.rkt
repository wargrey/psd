#lang typed/racket/base

(provide 0x40c)

(require "format.rkt")
(require "../parser.rkt")
(require "../draw.rkt")

(define 0x40c : (-> Integer Bytes String (List Positive-Real) PSD-Thumbnail)
  (lambda [id block name argl]
    (psd-thumbnail id name
                   (integer->thumbnail-format (parse-int32 block 0))
                   (parse-size block 4  4 positive-fixnum?) ; width
                   (parse-size block 8  4 positive-fixnum?) ; height
                   (parse-size block 12 4 positive-fixnum?) ; padded row bytes
                   (parse-size block 16 4 positive-fixnum?) ; total size
                   (parse-size block 20 4 positive-fixnum?) ; compressed size
                   (parse-size block 24 2 byte?)            ; bits per pixels
                   (parse-size block 26 2 index?)           ; number of planes
                   (make-object bitmap% (open-input-bytes (subbytes block 28))
                     'unknown #false #false (car argl)))))
