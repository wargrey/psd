#lang typed/racket/base

(provide psd-resource-parse!)

(require typed/racket/unsafe)
(require "../resource.rkt")

(module unsafe racket/base
  (provide (all-defined-out))

  (define (psd-resource-parser? func)
    (and (procedure? func)
         (eq? (procedure-arity func) 6))))

(unsafe-require/typed
 (submod "." unsafe)
 [psd-resource-parser? (-> Any Boolean : PSD-Resource-Parser)])

(define psd-resource-parsers : (HashTable Integer PSD-Resource-Parser) (make-hasheq))

(define psd-resource-parse! : (-> PSD-Image-Resources Integer Path-String (-> PSD-Resource-Parser PSD-Resource)
                                  (-> PSD-Resource) (-> exn:fail Any) (Option PSD-Resource))
  (lambda [resources id parser-dir do-with-parser fallback on-error]
    (define parser : PSD-Resource-Parser
      (hash-ref! psd-resource-parsers id
                 (λ [] (let ([id~a.rkt (build-path parser-dir (format "id~a.rkt" id))]
                             [0xFFFD (string->symbol (format "0x~x" id))])
                         (with-handlers ([exn? (λ [[e : exn]] (make-fallback-parser fallback))])
                           (assert (dynamic-require id~a.rkt 0xFFFD) psd-resource-parser?))))))
    (define resource : (Option PSD-Resource)
      (with-handlers ([exn:fail? (λ [[ef : exn:fail]] (and (on-error ef) #false))])
        (do-with-parser parser)))
    (cond [(not resource) (hash-remove! resources id)]
          [else (hash-set! resources id resource)])
    resource))

(define make-fallback-parser : (-> (-> PSD-Resource) PSD-Resource-Parser)
  (lambda [fallback]
    (λ [[id : Integer] [name : String] [bs : Bytes] [idx : Fixnum] [size : Index] [args : (Listof Any)]] : PSD-Resource
      (fallback))))
