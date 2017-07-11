#lang racket

(require "../main.rkt")
(require images/flomap)

(define test.psd (new psd% [path "test.psd"]))

(send test.psd desc)
(flomap->bitmap #:backing-scale 2.0 (send test.psd get-image))
