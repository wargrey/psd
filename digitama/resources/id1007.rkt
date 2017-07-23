#lang typed/racket/base

(provide 0x3ef)

(require "format.rkt")
(require "id1077.rkt")
(require "../exn.rkt")

(define 0x3ef : (-> Integer String Bytes Fixnum Index (List Boolean) PSD-Resource)
  (lambda [id name block idx size id1077?]
    (if (car id1077?)
        (throw-obsolete-error id "replaced by ~a" (psd-id->string #x0435))
        (0x435 id name block idx size null))))
