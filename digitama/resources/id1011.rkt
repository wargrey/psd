#lang typed/racket/base

(provide 0x3f3)

(require "format.rkt")
(require "../parser.rkt")

(define 0x3f3 : (-> Integer Bytes String Null PSD-Print-Flags)
  (lambda [id block name args]
    (PSD-Print-Flags id name
                     (> (parse-uint8 block 0) 0)
                     (> (parse-uint8 block 1) 0)
                     (> (parse-uint8 block 2) 0)
                     (> (parse-uint8 block 3) 0)
                     (> (parse-uint8 block 4) 0)
                     (> (parse-uint8 block 5) 0)
                     (> (parse-uint8 block 6) 0)
                     (> (parse-uint8 block 7) 0)
                     (> (parse-uint8 block 8) 0))))
