#lang typed/racket/base

(provide 0x3ff)

(require "format.rkt")
(require "../exn.rkt")

(define 0x3ff : (-> Integer Bytes String Null PSD-Resource)
  (lambda [id block name args]
    (throw-obsolete-error id)))
