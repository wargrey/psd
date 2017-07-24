#lang typed/racket/base

(provide (all-defined-out))

(require typed/racket/unsafe)
(require "../layer/format.rkt")

(module unsafe racket/base
  (provide (all-defined-out))

  (define (psd-layer-block-parser? func)
    (and (procedure? func)
         (eq? (procedure-arity func) 4))))

(unsafe-require/typed
 (submod "." unsafe)
 [psd-layer-block-parser? (-> Any Boolean : PSD-Layer-Block-Parser)])

(define psd-parse-layer-block : (-> Symbol (-> PSD-Layer-Block-Parser (Option PSD-Layer-Block)) (-> Nothing) (Option PSD-Layer-Block))
  (lambda [key do-with-parser otherwise]
    (define ~a.rkt : Path (collection-file-path (format "~a.rkt" key) "psd" "digitama" "layer" "blocks"))
    (define parse-resource (with-handlers ([exn? void]) (dynamic-require ~a.rkt key)))
    (cond [(psd-layer-block-parser? parse-resource) (do-with-parser parse-resource)]
          [else (otherwise)])))
