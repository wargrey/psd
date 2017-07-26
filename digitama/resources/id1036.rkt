#lang typed/racket/base

(require "format.rkt")
(require "../parser.rkt")
(require "../draw.rkt")

(unsafe-provide 0x40c)

(define 0x40c : (-> Integer String Bytes Fixnum Index (List Positive-Real) PSD-Thumbnail)
  (lambda [id name block idx size argl]
    (PSD-Thumbnail id name
                   (integer->thumbnail-format (parse-int32 block idx))
                   (parse-size block (fx+ idx 4)  4 positive-fixnum?) ; width
                   (parse-size block (fx+ idx 8)  4 positive-fixnum?) ; height
                   (parse-size block (fx+ idx 12) 4 positive-fixnum?) ; padded row bytes
                   (parse-size block (fx+ idx 16) 4 positive-fixnum?) ; total size
                   (parse-size block (fx+ idx 20) 4 positive-fixnum?) ; compressed size
                   (parse-size block (fx+ idx 24) 2 byte?)            ; bits per pixels
                   (parse-size block (fx+ idx 26) 2 index?)           ; number of planes
                   (make-object bitmap% (open-input-bytes (subbytes block (fx+ idx 28)))
                     'unknown #false #false (car argl)))))
