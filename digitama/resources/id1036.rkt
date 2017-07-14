#lang typed/racket/base

(provide 0x40c)

(require "format.rkt")
(require "../parser.rkt")
(require "../draw.rkt")

(define 0x40c : (-> Integer Bytes String (List Positive-Real) PSD-Thumbnail)
  (lambda [id block name argl]
    (psd-thumbnail id name
                   (integer->thumbnail-format (parse-int32 block 0))
                   (parse-size block 4  4 exact-positive-integer?)
                   (parse-size block 8  4 exact-positive-integer?)
                   (parse-size block 12 4 exact-positive-integer?)
                   (parse-size block 16 4 exact-positive-integer?)
                   (parse-size block 20 4 exact-positive-integer?)
                   (parse-size block 24 2 byte?)
                   (parse-size block 26 2 index?)
                   (make-object bitmap% (open-input-bytes (subbytes block 28))
                     'unknown #false #false (car argl)))))
