#lang typed/racket/base

(provide 0x435)

(require "format.rkt")

; Don't forget to update 1007
(define 0x435 : (-> Integer String Bytes Fixnum Index Null PSD-Resource)
  (lambda [id name block idx size argl]
    (PSD-Resource id name)))
