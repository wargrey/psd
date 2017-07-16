#lang typed/racket/base

(provide 0x3ef)

(require "format.rkt")
(require "id1077.rkt")
(require "../exn.rkt")

(define 0x3ef : (-> Integer Bytes String (List Boolean) PSD-Resource)
  (lambda [id block name id1077?]
    (if (car id1077?)
        (throw-obsolete-error id "replaced by 1077(0x0435)")
        (0x435 id block name null))))
