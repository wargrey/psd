#lang racket

(require "../psd.rkt")

(require racket/runtime-path)

(define-runtime-path tamer/ ".")
(define (psd? path) (regexp-match? #px"[^@][^2][^x][.]psd$" path))

(for/list ([file.psd (in-directory tamer/)] #:when (psd? file.psd))
  (define tamer.psd (read-psd file.psd #:try-@2x? #false #:backing-scale 2.0))
  (cons (path->string (file-name-from-path file.psd))
        tamer.psd))

#|
(for/list ([file.psd (in-directory "/Applications")] #:when (psd? file.psd))
  (printf "loading ~a~n" file.psd)
  (define bitmap.psd (with-handlers ([exn? values]) (read-psd file.psd #:try-@2x? #true)))
  (list file.psd (format "~a" bitmap.psd) bitmap.psd))
|#