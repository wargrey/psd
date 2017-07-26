#lang typed/racket/base

(require "format.rkt")
(require "../parser.rkt")

(unsafe-provide 0x3f3)

(define 0x3f3 : (-> Integer String Bytes Fixnum Index Null PSD-Resource)
  (lambda [id name block idx size args]
    (PSD-Print-Flags id name
                     (parse-boolean block idx)
                     (parse-boolean block (fx+ idx 1))
                     (parse-boolean block (fx+ idx 2))
                     (parse-boolean block (fx+ idx 3))
                     (parse-boolean block (fx+ idx 4))
                     (parse-boolean block (fx+ idx 5))
                     (parse-boolean block (fx+ idx 6))
                     (parse-boolean block (fx+ idx 7))
                     (parse-boolean block (fx+ idx 8)))))
