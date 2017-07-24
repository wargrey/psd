#lang typed/racket/base

(provide (all-defined-out))

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

(define psd-resource-parse! : (-> PSD-Image-Resources Integer Path-String (-> PSD-Resource-Parser PSD-Resource)
                                  (-> PSD-Resource) (-> exn:fail Any) (Option PSD-Resource))
  (lambda [resources id parser-dir do-with-parser fallback on-error]
    (define id~a.rkt : Path (build-path parser-dir (format "id~a.rkt" id)))
    (define parse : Symbol (string->symbol (format "0x~x" id)))
    (define 0xFFFD : Any (with-handlers ([exn? void]) (dynamic-require id~a.rkt parse)))
    (define resource : (Option PSD-Resource)
      (with-handlers ([exn:fail? (λ [[ef : exn:fail]] (and (on-error ef) #false))])
        (if (psd-resource-parser? 0xFFFD) (do-with-parser 0xFFFD) (fallback))))
    (cond [(not resource) (hash-remove! resources id)]
          [else (hash-set! resources id resource)])
    resource))
