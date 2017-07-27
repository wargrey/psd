#lang typed/racket/base

(require "../village/packbits.rkt")

(require typed/rackunit)
(require typed/rackunit/text-ui)

(define in : Bytes (bytes #xFE #xAA #x02 #x80 #x00 #x2A #xFD #xAA #x03 #x80 #x00 #x2A #x22 #xF7 #xAA))
(define out : Bytes (bytes #xAA #xAA #xAA #x80 #x00 #x2A #xAA #xAA #xAA #xAA #x80 #x00
                           #x2A #x22 #xAA #xAA #xAA #xAA #xAA #xAA #xAA #xAA #xAA #xAA))

(define tests-from-wikipedia
  (test-suite
   "Run-length Encoding"
   (check-equal? out (unpackbits in))
   (check-equal? out (unpackbits (bytes-length out) in))))

(run-tests tests-from-wikipedia)


(define out! : Bytes (make-bytes (bytes-length out)))
(define end (bytes-length in))
(collect-garbage)
(time (for ([i (in-range 100000)]) (unpackbits! out! 0 in 0 end)))
(collect-garbage)
(time (for ([i (in-range 100000)]) (unpackbits* in 0 end)))
