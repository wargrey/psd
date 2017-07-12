#lang typed/racket/base

(provide (all-defined-out))

(require racket/fixnum)

(define unpackbits : (-> Bytes Bytes)
  (lambda [bs]
    (cond [(bytes=? bs #"") #""]
          [else (let ([header (bytes-ref bs 0)])
                  (if (fx>= header 128)
                      (bytes-append (make-bytes (fx+ (fx- 256 header) 1) (bytes-ref bs 1))
                                    (unpackbits (subbytes bs 2)))
                      (bytes-append (subbytes bs 1 (fx+ header (fx+ 1 1)))
                                    (unpackbits (subbytes bs (fx+ header (fx+ 1 1)))))))])))
