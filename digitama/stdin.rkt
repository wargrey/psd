#lang typed/racket/base

(provide (all-defined-out))

(require racket/port)

(require "draw.rkt")
(require "image.rkt")

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
    (define version : Integer (read-integer /dev/psdin 2))
    
    (unless (and (equal? signature #"8BPS") (or (= version 1) (= version 2)))
      (raise-user-error 'read-psd-header "this is not a valid PSD/PSB file: ~a" (object-name /dev/psdin)))
    
    (read-nbytes* /dev/psdin 6) ; reserved
    (values (if (fx= version 1) 4 8)
            (read-integer /dev/psdin 2 positive-byte?)   ; channels
            (read-integer /dev/psdin 4 positive-index?)  ; height
            (read-integer /dev/psdin 4 positive-index?)  ; width
            (read-integer /dev/psdin 2 positive-byte?)   ; depth
            (integer->color-mode (read-integer /dev/psdin 2)))))

(define read-psd-subsection : (-> Input-Port Positive-Byte (Values Bytes Bytes (Option Bytes) (Option Bytes) (Option Bytes)))
  (lambda [/dev/psdin psd/psb-size]
    (define color-mode-data : Bytes (read-n:bytes /dev/psdin 4))
    (define images-resources : Bytes (read-n:bytes /dev/psdin 4))
    (define layer+mask-size : Index (read-integer /dev/psdin psd/psb-size index?))
    (if (fx= layer+mask-size 0)
        (values color-mode-data images-resources #false #false #false)
        (let ([layer-info : Bytes (read-n:bytes /dev/psdin psd/psb-size)]
              [global-mask-info : Bytes (read-n:bytes /dev/psdin 4)])
          (define layer-size : Index (bytes-length layer-info))
          (define mask-size : Index (bytes-length global-mask-info))
          (define tagged-blocks-size : Fixnum (fx- layer+mask-size (fx+ (fx+ psd/psb-size layer-size) (fx+ 4 mask-size))))
          (values color-mode-data images-resources
                  (and (fx> layer-size 0) layer-info)
                  (and (fx> mask-size 0) global-mask-info)
                  (and (fx> tagged-blocks-size 0) (read-nbytes* /dev/psdin tagged-blocks-size)))))))

(define read-psd-composite-image : (-> Input-Port (Values PSD-Compression-Method Bytes))
  (lambda [/dev/psdin]
    (values (integer->compression-method (read-integer /dev/psdin 2))
            (port->bytes /dev/psdin))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define read-n:bytes : (-> Input-Port Fixnum Bytes)
  (lambda [/dev/psdin size]
    (read-nbytes* /dev/psdin (read-integer /dev/psdin size))))

(define read-integer : (All (a) (case-> [Input-Port Fixnum -> Integer]
                                        [Input-Port Fixnum (-> Any Boolean : a) -> a]))
  (case-lambda
    [(/dev/psdin bsize) (integer-bytes->integer (read-bytes* /dev/psdin bsize) #false #true 0 bsize)]
    [(/dev/psdin bsize subinteger?) (assert (read-integer /dev/psdin bsize) subinteger?)]))

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
