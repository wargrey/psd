#lang typed/racket/base

(provide 0x424)

(require racket/string)

(require "format.rkt")
(require "../parser.rkt")
(require "../draw.rkt")

(define 0x424 : (-> Integer Bytes String Null PSD-File-Info)
  (lambda [id block name argl]
    (psd-file-info id name block)))
