#lang racket

(require "../main.rkt")

(require racket/runtime-path)

(define-runtime-path ./test.psd "test.psd")

(define test.psd (read-psd ./test.psd))
test.psd
;(send test.psd desc)
;(flomap->bitmap #:backing-scale 2.0 (send test.psd get-image))
