#lang typed/racket/base

(provide (all-defined-out))

(require "resource.rkt")

(define throw-unsupported-error : (-> Symbol String Any * Nothing)
  (lambda [func fmt . args]
    (raise (make-exn:fail:unsupported (apply format (string-append "~a: " fmt) func args)
                                      (continuation-marks #false)))))

(define throw-obsolete-error : (-> Integer Any * Nothing)
  (lambda [id . args]
    (define-values (idstr message) (values id args))
    (raise (make-exn:fail:unsupported (format "obsolete resource: ~a" message)
                                      (continuation-marks #false)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define psd-id->string : (-> Integer String)
  (lambda [id]
    (define ID : String (string-upcase (format (if (< id #x1000) "0~x" "~x") id)))
    (string-append "0x" ID "(" (number->string id) ")")))

(define psd-args->message : (-> Integer (Listof Any) (Values String String))
  (lambda [id args]
    (define idstr : String (psd-id->string id))
    (define message : String
      (cond [(null? args) idstr]
            [(string? (car args)) (apply format (string-append idstr ": " (car args)) (cdr args))]
            [else (format (string-append idstr ": ~s") args)]))
    (values idstr message)))

(define psd-warn-broken-resource : (-> exn Void)
  (lambda [e]
    (log-message (current-logger) 'warning 'exn:psd (exn-message e) e)))

(define psd-warn-broken-information : (-> exn Void)
  (lambda [e]
    (log-message (current-logger) 'warning 'exn:psd (exn-message e) e)))
