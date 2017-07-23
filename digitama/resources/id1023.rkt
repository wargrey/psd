#lang typed/racket/base

(provide 0x3ff)

(require "format.rkt")
(require "../exn.rkt")

(define 0x3ff : (-> Integer String Bytes Fixnum Index Null PSD-Resource)
  (lambda [id name block idx size args]
    (throw-obsolete-error id)))
