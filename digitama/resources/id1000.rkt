#lang typed/racket/base

(provide 0x3e8)

(require "format.rkt")
(require "../exn.rkt")

(define 0x3e8 : (-> Integer Bytes String Null PSD-Resource)
  (lambda [id block name argl]
    (throw-obsolete-error id "Photoshop 2.0 only")))
