#lang typed/racket/base

(provide (all-defined-out))

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

(define psd-layer-info-parse! : (-> PSD-Layer-Infobase Symbol Path-String (-> PSD-Layer-Info-Parser PSD-Layer-Info)
                                    (-> PSD-Layer-Info) (-> exn:fail Any) (Option PSD-Layer-Info))
  (lambda [infobase key parser-dir do-with-parser fallback on-error]
    (define ~a.rkt : Path (build-path parser-dir (format "~a.rkt" key)))
    (define fffd : Any (with-handlers ([exn? void]) (dynamic-require ~a.rkt key)))
    (define info : (Option PSD-Layer-Info)
      (with-handlers ([exn:fail? (λ [[ef : exn:fail]] (and (on-error ef) #false))])
        (if (psd-layer-info-parser? fffd) (do-with-parser fffd) (fallback))))
    (cond [(not info) (hash-remove! infobase key)]
          [else (hash-set! infobase key info)])
    info))
