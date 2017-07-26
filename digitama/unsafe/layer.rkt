#lang typed/racket/base

(provide psd-layer-info-parse!)

(require typed/racket/unsafe)
(require "../layer/format.rkt")

(module unsafe racket/base
  (provide (all-defined-out))

  (define (psd-layer-info-parser? func)
    (and (procedure? func)
         (eq? (procedure-arity func) 4))))

(unsafe-require/typed
 (submod "." unsafe)
 [psd-layer-info-parser? (-> Any Boolean : PSD-Layer-Info-Parser)])

(define psd-layer-info-parsers : (HashTable Symbol PSD-Layer-Info-Parser) (make-hasheq))

(define psd-layer-info-parse! : (-> PSD-Layer-Infobase Symbol Path-String (-> PSD-Layer-Info-Parser PSD-Layer-Info)
                                    (-> PSD-Layer-Info) (-> exn:fail Any) (Option PSD-Layer-Info))
  (lambda [infobase key parser-dir do-with-parser fallback on-error]
    (define parser : PSD-Layer-Info-Parser
      (hash-ref! psd-layer-info-parsers key
                 (λ [] (let ([~a.rkt (build-path parser-dir (format "~a.rkt" key))])
                         (with-handlers ([exn? (λ [[e : exn]] (make-fallback-parser fallback))])
                           (assert (dynamic-require ~a.rkt key) psd-layer-info-parser?))))))
    (define info : (Option PSD-Layer-Info)
      (with-handlers ([exn:fail? (λ [[ef : exn:fail]] (and (on-error ef) #false))])
        (do-with-parser parser)))
    (cond [(not info) (hash-remove! infobase key)]
          [else (hash-set! infobase key info)])
    info))

(define make-fallback-parser : (-> (-> PSD-Layer-Info) PSD-Layer-Info-Parser)
  (lambda [fallback]
    (λ [[bs : Bytes] [idx : Fixnum] [size : Index] [args : (Listof Any)]] : PSD-Layer-Info
      (fallback))))
