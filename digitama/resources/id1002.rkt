#lang typed/racket/base

(provide 0x3ea)

(require "format.rkt")
(require "../exn.rkt")

(define 0x3ea : (-> Integer Bytes String Null PSD-Resource)
  (lambda [id block name argl]
    (throw-obsolete-error id "No longer read by Photoshop")))
