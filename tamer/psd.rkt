#lang racket

(require "../main.rkt")
(require "../profile.rkt")

(require racket/runtime-path)

(define-runtime-path tamer/ "samples")
(define (psd? path) (regexp-match? #px"[^@][^2][^x][.]psd$" path))

(for/list ([file.psd (in-directory tamer/)] #:when (psd? file.psd))
  (define tamer.psd (read-psd file.psd #:try-@2x? #false #:backing-scale 2.0))
  (psd-profile tamer.psd #:resolve? #true)
  (list (path->string (file-name-from-path file.psd))
        (psd-layers tamer.psd #:resolve? #true)
        (psd-global-layer-mask tamer.psd)
        tamer.psd
        (psd-image-resources tamer.psd #:resolve? #true)))

#;(for ([file.psd (in-directory (build-path (find-system-path 'home-dir) "Gyoudmon"))] #:when (psd? file.psd))
  (define bitmap.psd (read-psd file.psd #:try-@2x? #true #:backing-scale 2.0))
  (psd-profile bitmap.psd))
