#lang typed/racket/base

(provide 0x424)

(require racket/fixnum)
(require racket/string)

(require "format.rkt")

(define 0x424 : (-> Integer String Bytes Fixnum Index Null PSD-File-Info)
  (lambda [id name xmp idx size argl]
    (PSD-File-Info id name (subbytes xmp idx (fx+ idx size)))))
