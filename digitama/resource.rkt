#lang typed/racket/base

(provide (all-defined-out))

(require "misc.rkt")

(define-type PSD-Resource-Block (Pairof String Special-Comment))
(define-type PSD-Image-Resources (HashTable Integer (U PSD-Resource-Block PSD-Resource)))
(define-type PSD-Resource-Parser (-> Integer Bytes String (Listof Any) PSD-Resource))

(define psd-empty-resources : PSD-Image-Resources (make-hasheq))

(struct PSD-Resource
  ([id : Integer]
   [name : String])
  #:transparent)
