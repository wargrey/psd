#lang typed/racket/base

(provide psd-resource-constructor?)

(require typed/racket/unsafe)
(require "../resource.rkt")

(module unsafe racket/base
  (provide (all-defined-out))

  (define (psd-resource-constructor? func)
    (and (procedure? func)
         (eq? (procedure-arity func) 4))))

(unsafe-require/typed
 (submod "." unsafe)
 [psd-resource-constructor? (-> Any Boolean : PSD-Resource-Constructor)])
