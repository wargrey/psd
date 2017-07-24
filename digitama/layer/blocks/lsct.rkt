#lang typed/racket/base

(provide (all-defined-out))

(require "../../parser.rkt")
(require "../format.rkt")

(define lsct : (-> Bytes Fixnum Fixnum Null PSD-Layer-Section-Divider)
  (lambda [layer-info start size argl]
    (PSD-Layer-Section-Divider (parse-uint32 layer-info start)
                               (and (fx>= size 12) (parse-keyword layer-info (fx+ start 8)))
                               (and (fx>= size 16) (parse-uint32 layer-info (fx+ start 12))))))
