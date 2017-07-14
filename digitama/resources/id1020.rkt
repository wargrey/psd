#lang typed/racket/base

(provide 0x3fc)

(require "format.rkt")
(require "misc.rkt")

(define 0x3fc : (-> Integer Bytes String Null PSD-Resource)
  (lambda [id block name args]
    (throw-obsolete-error id)))
