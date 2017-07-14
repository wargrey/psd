#lang typed/racket/base

(provide 0x424)

(require racket/string)

(require "format.rkt")

(define 0x424 : (-> Integer Bytes String Null PSD-File-Info)
  (lambda [id xmp name argl]
    (psd-file-info id name xmp)))
