#lang typed/racket/base

(provide 0x435)

(require "format.rkt")

; do not forget update 1007
(define 0x435 : (-> Integer Bytes String Null PSD-Resource)
  (lambda [id block name argl]
    (psd-resource id name)))
