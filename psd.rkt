#lang typed/racket/base

;;; http://www.adobe.com/devnet-apps/photoshop/fileformatashtml

(provide (all-defined-out))

(require "digitama/parser.rkt")
(require "digitama/psd.rkt")
(require "digitama/misc.rkt")
(require "digitama/unsafe/bitmap.rkt")

(struct: psd : PSD psd-header
  ([color : Bytes]
   [resources : Bytes]
   [layers : Bytes]
   [compression : PSD-Compression-Mode]
   [image : (Boxof (U Bytes (Instance Bitmap%)))])
  #:extra-constructor-name make-psd
  #:property prop:convertible
  (λ [[self : PSD] [request : Symbol] [default : Any]]
    (convert (psd->bitmap self) request default)))

(define read-psd : (->* ((U Path-String Input-Port)) (Boolean) PSD)
  (lambda [/dev/psdin [try-@2x? #false]]
    (if (not (input-port? /dev/psdin))
        (call-with-input-file* (select-psd-file /dev/psdin try-@2x?) read-psd)
        (let*-values ([(version channels height width depth color-mode) (read-psd-header /dev/psdin)]
                      [(color-mode-data image-resources layer+mask-info compression image-data) (read-psd-section /dev/psdin version)])
          (make-psd (integer->version version) channels width height depth color-mode
                    color-mode-data image-resources layer+mask-info compression
                    (box image-data))))))

(define read-psd-as-bitmap : (->* ((U Path-String Input-Port)) (Boolean) (Instance Bitmap%))
  (lambda [/dev/psdin [try-@2x? #false]]
    (if (not (input-port? /dev/psdin))
        (call-with-input-file* (select-psd-file /dev/psdin try-@2x?) read-psd-as-bitmap)
        (let*-values ([(version channels height width depth color-mode) (read-psd-header /dev/psdin)]
                      [(_cmd _ir _lmi compression image-data) (read-psd-section /dev/psdin version)])
          (psd-image-data->bitmap 'read-psd-as-bitmap image-data width height channels compression 2.0)))))

(define psd->bitmap : (-> PSD (Instance Bitmap%))
  (lambda [self]
    (define &bmp : (Boxof (U Bytes (Instance Bitmap%))) (psd-image self))
    (define maybe-bmp : (U Bytes (Instance Bitmap%)) (unbox &bmp))
    (cond [(not (bytes? maybe-bmp)) maybe-bmp]
          [else (let ([bmp (psd-image-data->bitmap 'psd->bitmap maybe-bmp
                                                   (psd-header-width self) (psd-header-height self)
                                                   (psd-header-channels self) (psd-compression self)
                                                   2.0)])
                  (set-box! &bmp bmp)
                  bmp)])))

(define psd-size : (-> PSD (Values Positive-Fixnum Positive-Fixnum))
  (lambda [this]
    (values (psd-header-width this)
            (psd-header-height this))))

#;(define psd-desc : (->* () (Output-Port) Void)
  (lambda [[out (current-output-port)]]
    (fprintf out (foldr string-append ""
                        (add-between (list "~a Object:" "Size: [~a * ~a]" "Channels: ~a" "Depth: ~a" "Color Mode: ~a"
                                           "Compression Method: ~a" "Resources Count: ~a~a" "Global Mask: ~a" "Tagged Blocks: ~a~a~n")
                                     "~n    "))
             (case ~version [{1} 'PSD] [{2} 'PSB]) ~width ~height ~channels ~depth
             (list-ref '{Bitmap Grayscale Indexed RGB CMYK Multichannel Duotone Lab} ~color-mode)
             (list-ref '{Raw RLE ZIP-no-prediction ZIP-with-prediction} ~compression)
             (hash-count image-resources) (hash-keys image-resources)
             (cond [(zero? (hash-count global-mask)) "None"]
                   [(= 128 (hash-ref global-mask 'kind)) "Use value stored per layer"]
                   [else global-mask])
             (hash-count tagged-blocks) (hash-keys tagged-blocks))
    
    (fprintf out "~n    Layer Count: ~a~n" (vector-length layers))
    (for ([index (in-range (sub1 (vector-length layers)) -1 -1)])
      (send (vector-ref layers index) folder?)
      (send (vector-ref layers index) desc "        " out)
      (newline))))
