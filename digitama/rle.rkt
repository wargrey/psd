#lang racket

(provide unpackbits)

{module+ test
  (require rackunit)
  (require rackunit/text-ui)}

(define unpackbits
  {lambda [bs]
    (cond [(zero? (bytes-length bs)) #""]
          [else (let ([header (bytes-ref bs 0)])
                  (cond [(>= header 128) (bytes-append (make-bytes (add1 (- 256 header)) (bytes-ref bs 1)) (unpackbits (subbytes bs 2)))]
                        [else (bytes-append (subbytes bs 1 (+ header 1 1)) (unpackbits (subbytes bs (+ header 1 1))))]))])})

{module+ test
  (define-test-suite wikipedia-testcase
    (let ([input (bytes #xFE #xAA #x02 #x80 #x00 #x2A #xFD #xAA #x03 #x80 #x00 #x2A #x22 #xF7 #xAA)]
          [output (bytes #xAA #xAA #xAA #x80 #x00 #x2A #xAA #xAA #xAA #xAA #x80 #x00 #x2A #x22 #xAA #xAA #xAA #xAA #xAA #xAA #xAA #xAA #xAA #xAA)])
      (check-equal? output (unpackbits input))))
  
  (run-tests wikipedia-testcase)}
