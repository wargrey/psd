#lang typed/racket/base

(provide 0x3eb)

(require "format.rkt")
(require "../exn.rkt")

(define 0x3eb : (-> Integer Bytes String Null PSD-Resource)
  (lambda [id block name argl]
    (throw-obsolete-error id "Photoshop 2.0 only")))
