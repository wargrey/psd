#lang typed/racket/base

(provide (all-defined-out))

(define unpackbits : (-> Bytes Bytes)
  (lambda [bs]
    (cond [(bytes=? bs #"") #""]
          [else (let ([header (bytes-ref bs 0)])
                  (if (>= header 128)
                      (bytes-append (make-bytes (+ (- 256 header) 1) (bytes-ref bs 1))
                                    (unpackbits (subbytes bs 2)))
                      (bytes-append (subbytes bs 1 (+ header 1 1))
                                    (unpackbits (subbytes bs (+ header 1 1))))))])))
