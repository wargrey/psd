#lang typed/racket/base

(provide (all-defined-out))

(require typed/racket/unsafe)
(require "../resource.rkt")

(module unsafe racket/base
  (provide (all-defined-out))

  (define (psd-resource-parser? func)
    (and (procedure? func)
         (eq? (procedure-arity func) 4))))

(unsafe-require/typed
 (submod "." unsafe)
 [psd-resource-parser? (-> Any Boolean : PSD-Resource-Parser)])

(define psd-parse-resource : (-> Integer (-> PSD-Resource-Parser (Option PSD-Resource)) (-> Nothing) (Option PSD-Resource))
  (lambda [id do-with-parser otherwise]
    (define id~a.rkt : Path (collection-file-path (format "id~a.rkt" id) "psd" "digitama" "resources"))
    (define parse : Symbol (string->symbol (format "0x~x" id)))
    (define parse-resource (with-handlers ([exn? void]) (dynamic-require id~a.rkt parse)))
    (cond [(psd-resource-parser? parse-resource) (do-with-parser parse-resource)]
          [else (otherwise)])))
