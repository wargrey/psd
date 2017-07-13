#lang typed/racket/base

(provide (all-defined-out))

(require racket/fixnum)
(require racket/port)

(require "misc.rkt")
(require "parser.rkt")
(require "packbits.rkt")
(require "unsafe/bitmap.rkt")

(struct psd-header
  ([version : PSD-Version]
   [channels : Positive-Byte]
   [width : Positive-Index]
   [height : Positive-Index]
   [depth : Positive-Byte]
   [color : PSD-Color-Mode])
  #:transparent)

(struct psd-data psd-header
  ([compression : PSD-Compression-Mode]
   [density : Positive-Real])
  #:transparent)

(define-enumeration* psd-version #:+> PSD-Version ; order matters
  version->integer integer->version
  [1 PSD PSB])

(define-enumeration* psd-color-mode #:+> PSD-Color-Mode ; order matters
  color-mode->integer integer->color-mode
  [0 Bitmap Grayscale Indexed RGB CMYK Multichannel Duotone Lab])

(define-enumeration* psd-compression-mode #:+> PSD-Compression-Mode ; order matters
  compression-mode->integer integer->compression-mode
  [0 Raw RLE ZIP ZIP/prediction])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
    
    (read-nbytes* /dev/psdin 6)                          ; reserved
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
(define psd-image-data->bitmap : (-> Symbol Bytes Positive-Index Positive-Index Positive-Byte
                                     PSD-Compression-Mode Positive-Real (Instance Bitmap%))
  (lambda [func planar-data width height channels compression density]
    (unless (or (fx= channels 3) (fx= channels 4)) (throw-unsupported-error func "inproper channel count: ~a" channels))
    (case compression
      [(Raw) (planar-data->bitmap planar-data width height channels density)]
      [(RLE) (let* ([data-index (assert (fx* (fx* height channels) 2) index?)]
                    [scan-lines (fxquotient data-index 2)])
               (define sizes : (Listof Index) (parse-nsizes-list planar-data scan-lines 2 0))
               (define intervals : (Listof (Pairof Integer Integer)) (nbytes-pairs sizes data-index))
               (planar-data->bitmap (for/list : (Listof Bytes) ([interval (in-list intervals)])
                                      (unpackbits width planar-data (car interval) (cdr interval)))
                                    width height channels density))]
      [else (throw-unsupported-error func "unimplemented compression method: ~a" compression)])))

(define throw-unsupported-error : (-> Symbol String Any * Nothing)
  (lambda [func fmt . args]
    (raise (make-exn:fail:unsupported (apply format (string-append "~a: " fmt) func args)
                                      (continuation-marks #false)))))
