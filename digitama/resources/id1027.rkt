#lang typed/racket/base

(provide 0x403)

(require "format.rkt")
(require "../exn.rkt")

(define 0x403 : (-> Integer Bytes String Null PSD-Resource)
  (lambda [id block name args]
    (throw-obsolete-error id)))
