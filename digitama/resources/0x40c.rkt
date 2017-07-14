#lang typed/racket/base

(provide make-resource)

(require "format.rkt")
(require "../parser.rkt")
(require "../draw.rkt")

(define make-resource : (-> Integer Bytes String (List Positive-Real) PSD-Thumbnail)
  (lambda [id block name argl]
    (psd-thumbnail id name
                   (integer->thumbnail-format (parse-integer block 4 #false 0))
                   (parse-integer block 4 #false exact-positive-integer? 4)
                   (parse-integer block 4 #false exact-positive-integer? 8)
                   (parse-integer block 4 #false exact-positive-integer? 12)
                   (parse-integer block 4 #false exact-positive-integer? 16)
                   (parse-integer block 4 #false exact-positive-integer? 20)
                   (parse-integer block 2 #false byte? 24)
                   (parse-integer block 2 #false byte? 26)
                   (make-object bitmap% (open-input-bytes (subbytes block 28))
                     'unknown #false #false (car argl)))))
