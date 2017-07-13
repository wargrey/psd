#lang typed/racket/base

(provide (all-defined-out))

(require racket/port)

(require "psd.rkt")
(require "draw.rkt")

(define select-psd-file : (-> Path-String Positive-Real Boolean (Values Path-String Positive-Real))
  (lambda [src.psd density try?]
    (cond [(not try?) (values src.psd density)]
          [else (let* ([path.psd : String (if (string? src.psd) src.psd (path->string src.psd))]
                       [path@2x.psd : String (regexp-replace #rx"([.][^.]*|)$" path.psd "@2x\\1")])
                  (cond [(not (file-exists? path@2x.psd)) (values path.psd density)]
                        [else (values path@2x.psd (+ density density))]))])))

(define read-psd-header : (-> Input-Port (Values Positive-Byte Positive-Byte Positive-Index Positive-Index Positive-Byte PSD-Color-Mode))
  (lambda [/dev/psdin]
    (define signature : Bytes (read-nbytes* /dev/psdin 4))
    (define version : Integer (read-integer /dev/psdin 2 #false))
    
    (unless (and (equal? signature #"8BPS") (or (= version 1) (= version 2)))
      (raise-user-error 'read-psd-header "this is not a valid PSD/PSB file: ~a" (object-name /dev/psdin)))
    
    (read-nbytes* /dev/psdin 6) ; reserved
    (values version
            (read-integer /dev/psdin 2 #false positive-byte?)   ; channels
            (read-integer /dev/psdin 4 #false positive-index?)  ; height
            (read-integer /dev/psdin 4 #false positive-index?)  ; width
            (read-integer /dev/psdin 2 #false positive-byte?)   ; depth
            (integer->color-mode (read-integer /dev/psdin 2 #false)))))

(define read-psd-section : (-> Input-Port Byte (Values Bytes Bytes Bytes PSD-Compression-Mode Bytes))
  (lambda [/dev/psdin version]
    (values (read-n:bytes /dev/psdin 4)               ; color mode data
            (read-n:bytes /dev/psdin 4)               ; image resources
            (read-n:bytes /dev/psdin (fx* 4 version)) ; layer+mask information
            (integer->compression-mode (read-integer /dev/psdin 2 #false))
            (port->bytes /dev/psdin))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define read-n:bytes : (-> Input-Port Fixnum Bytes)
  (lambda [/dev/psdin size]
    (read-nbytes* /dev/psdin (read-integer /dev/psdin size #false))))

(define read-integer : (All (a) (case-> [Input-Port Fixnum Boolean -> Integer]
                                        [Input-Port Fixnum Boolean (-> Any Boolean : a) -> a]))
  (case-lambda
    [(/dev/psdin bsize signed?) (integer-bytes->integer (read-bytes* /dev/psdin bsize) signed? #true 0 bsize)]
    [(/dev/psdin bsize signed? subinteger?) (assert (read-integer /dev/psdin bsize signed?) subinteger?)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define read-bytes* : (-> Input-Port Integer Bytes)
  (lambda [/dev/psdin size]
    (define bs : (U Bytes EOF) (read-bytes size /dev/psdin))
    (if (bytes? bs) bs (throw-eof-error /dev/psdin))))

(define read-nbytes* : (-> Input-Port Integer Bytes)
  (lambda [/dev/psdin size]
    (define bs : (U Bytes EOF) (read-bytes size /dev/psdin))
    (cond [(and (bytes? bs) (= (bytes-length bs) size)) bs]
          [else (throw-eof-error /dev/psdin)])))

(define throw-eof-error : (-> Input-Port Nothing)
  (lambda [/dev/psdin]
    (raise (make-exn:fail:read:eof "unexpected end of file!"
                                   (continuation-marks #false)
                                   null))))
