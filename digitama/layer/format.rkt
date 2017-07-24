#lang typed/racket/base

(provide (all-defined-out))

(define-type PSD-Layer-Segment (Vector Special-Comment Fixnum Index))
(define-type PSD-Layer-Blocks (HashTable Symbol (U PSD-Layer-Segment PSD-Layer-Block)))
(define-type PSD-Layer-Block-Parser (-> Bytes Fixnum Fixnum (Listof Any) PSD-Layer-Block))

(struct PSD-Layer-Block () #:transparent)

(struct PSD-Layer-Unicode-Name PSD-Layer-Block
  ([data : String])
  #:transparent)

(struct PSD-Layer-Section-Divider PSD-Layer-Block
  ([type : Integer]
   [blend-mode : (Option Symbol)]
   [subtype : (Option Integer)])
  #:transparent)
