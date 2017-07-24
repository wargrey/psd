#lang typed/racket/base

(provide (all-defined-out))

(require "../../parser.rkt")
(require "../format.rkt")

(define luni : (-> Bytes Fixnum Fixnum Null PSD-Layer-Unicode-Name)
  (lambda [layer-info start size argl]
    (define-values (name idx) (parse-unicode-string layer-info start))
    (PSD-Layer-Unicode-Name name)))
