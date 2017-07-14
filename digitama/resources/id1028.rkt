#lang typed/racket/base

(provide 0x404)

(require "format.rkt")

(define 0x404 : (-> Integer Bytes String Null PSD-File-Info)
  (lambda [id iptc name argl]
    (psd-file-info id name iptc)))
