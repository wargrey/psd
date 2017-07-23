#lang typed/racket/base

(provide (all-defined-out))

(define-type PSD-Resource-Block (Vector String Special-Comment Fixnum Index))
(define-type PSD-Image-Resources (HashTable Integer (U PSD-Resource-Block PSD-Resource)))
(define-type PSD-Resource-Parser (-> Integer String Bytes Fixnum Index (Listof Any) PSD-Resource))

(struct PSD-Resource
  ([id : Integer]
   [name : String])
  #:transparent)
