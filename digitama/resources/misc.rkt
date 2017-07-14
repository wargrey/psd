#lang typed/racket/base

(provide (all-defined-out))

(define throw-obsolete-error : (-> Integer Any * Nothing)
  (lambda [id . args]
    (define idstr : String (string-append "0x" (string-upcase (format (if (< id #x1000) "0~x" "~x") id))))
    (define message : String
      (cond [(null? args) idstr]
            [(string? (car args)) (apply format (string-append idstr ": " (car args)) (cdr args))]
            [else (format (string-append idstr ": ~s") args)]))
    (raise (make-exn:fail:unsupported (format "obsolete resource: ~a" message) (continuation-marks #false)))))
