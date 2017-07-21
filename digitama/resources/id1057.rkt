#lang typed/racket/base

(provide 0x421)

(require racket/fixnum)

(require "format.rkt")
(require "../parser.rkt")

(define 0x421 : (-> Integer Bytes String Null PSD-Version-Info)
  (lambda [id iptc-naa name argl]
    (define-values (writer wsize) (parse-unicode-string iptc-naa 5))
    (define reader-start : Fixnum (fx+ 5 (fx+ wsize 4)))
    (define-values (reader rsize) (parse-unicode-string iptc-naa reader-start))
    (PSD-Version-Info id name
                      (parse-uint32 iptc-naa 0 index?)
                      (fx> (parse-uint8 iptc-naa 4) 0)
                      writer reader
                      (parse-uint32 iptc-naa (fx+ reader-start (fx+ rsize 4)) index?))))
