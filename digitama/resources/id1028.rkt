#lang typed/racket/base

(provide 0x404)

(require "format.rkt")
(require "../parser.rkt")
(require "../draw.rkt")

(define 0x404 : (-> Integer Bytes String Null PSD-File-Info)
  (lambda [id block name argl]
    (psd-file-info id name block)))
