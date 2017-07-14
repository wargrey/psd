#lang typed/racket/base

(provide (all-defined-out))

(require "misc.rkt")

(define-type PSD-Resource-Block (Pairof String Special-Comment))
(define-type PSD-Image-Resources (HashTable Integer (U PSD-Resource-Block PSD-Resource)))
(define-type PSD-Resource-Parser (-> Integer Bytes String (Listof Any) PSD-Resource))

(define psd-empty-resources : PSD-Image-Resources (make-hasheq))

(struct: psd-resource : PSD-Resource
  ([id : Integer]
   [name : String]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define psd-assert : (All (a) (-> Any (-> Any Boolean : a) (Option a)))
  (lambda [v psd-resource?]
    (and v (assert v psd-resource?))))

(define psd-warn-broken-resource : (-> exn False)
  (lambda [e]
    (log-message (current-logger) 'warning 'exn:psd:resource (exn-message e) e)
    #false))
