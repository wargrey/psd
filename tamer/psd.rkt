#lang racket

(require "../main.rkt")

(require racket/runtime-path)

(define-runtime-path tamer/ "./samples")
(define (psd? path) (regexp-match? #px"[^@][^2][^x][.]psd$" path))

(psd-remove-broken-resource? #true)
(psd-remove-unknown-resource? #false)

(for/list ([file.psd (in-directory tamer/)] #:when (psd? file.psd))
  (define tamer.psd (read-psd file.psd #:try-@2x? #false #:backing-scale 2.0))
  (list (path->string (file-name-from-path file.psd))
        (psd-layers tamer.psd)
        (psd-global-layer-mask tamer.psd)
        tamer.psd
        (psd-resources* tamer.psd)))

#;(for/list ([file.psd (in-directory (build-path (find-system-path 'home-dir) "Gyoudmon"))] #:when (psd? file.psd))
  (printf "loading ~a~n" file.psd)
  (define bitmap.psd (read-psd file.psd #:try-@2x? #true #:backing-scale 2.0))
  (cons file.psd bitmap.psd))
