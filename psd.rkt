#lang typed/racket/base

;;; http://www.adobe.com/devnet-apps/photoshop/fileformatashtml

(provide (all-defined-out))

(require "digitama/psd.rkt")
(require "digitama/draw.rkt")
(require "digitama/stdin.rkt")
(require "digitama/bitmap.rkt")
(require "digitama/misc.rkt")

(struct: psd : PSD psd-section ([density : Positive-Real])
  #:transparent #:constructor-name use-read-psd-instead
  #:property prop:convertible
  (λ [[self : PSD] [request : Symbol] [default : Any]]
    (with-handlers ([exn? (λ [[e : exn]] #false)])
      (convert (psd->bitmap self) request default))))

(define read-psd : (->* ((U Path-String Input-Port)) (#:backing-scale Positive-Real #:try-@2x? Boolean) PSD)
  (lambda [/dev/psdin #:backing-scale [density 1.0] #:try-@2x? [try-@2x? #false]]
    (if (input-port? /dev/psdin)
        (let*-values ([(version channels height width depth color-mode) (read-psd-header /dev/psdin)]
                      [(color-mode-data image-resources layer+mask-info compression-mode image-data) (read-psd-section /dev/psdin version)])
          (use-read-psd-instead (integer->version version) channels width height depth color-mode 
                                (make-special-comment color-mode-data)
                                (make-special-comment image-resources)
                                (make-special-comment layer+mask-info)
                                compression-mode (make-special-comment image-data) density))
        (let-values ([(path scale) (select-psd-file /dev/psdin density try-@2x?)])
          (call-with-input-file* path (λ [[psdin : Input-Port]] (read-psd psdin #:backing-scale scale)))))))

(define read-psd-as-bitmap : (->* ((U Path-String Input-Port)) (#:backing-scale Positive-Real #:try-@2x? Boolean) (Instance Bitmap%))
  (lambda [/dev/psdin #:backing-scale [density 1.0] #:try-@2x? [try-@2x? #false]]
    (if (input-port? /dev/psdin)
        (let*-values ([(version channels height width depth color-mode) (read-psd-header /dev/psdin)]
                      [(_cmd _ir _lmi compression image-data) (read-psd-section /dev/psdin version)])
          (psd-image-data->bitmap 'read-psd-as-bitmap image-data color-mode width height channels depth compression density))
        (let-values ([(path scale) (select-psd-file /dev/psdin density try-@2x?)])
          (call-with-input-file* path (λ [[psdin : Input-Port]] (read-psd-as-bitmap psdin #:backing-scale scale)))))))

#;(define psd-image-resources : (-> PSD PSD-Image-Resources)
  (lambda [self]
    (define maybe-res : (U (Instance Bitmap%) Special-Comment) (psd-section-resources self))
    (cond [(not (special-comment? maybe-res)) maybe-res]
          [else (let ([resource-data : Bytes (assert (special-comment-value maybe-res) bytes?)])
                  (define bmp : (Instance Bitmap%)
                    (psd-image-data->bitmap 'psd->bitmap image-data (psd-header-color-mode self)
                                            (psd-header-width self) (psd-header-height self)
                                            (psd-header-channels self) (psd-header-depth self)
                                            (psd-section-compression-mode self) (psd-density self)))
                  (set-psd-section-image! self bmp)
                  bmp)])
    (with-input-from-bytes resource-data
        {thunk (define read-record
                 {lambda [res]
                   (let ([skip-8BIM (read-bytes 4 res)]
                         [id (read-int 2 res)])
                     (cons id (mcons (second (regexp-match #px#"((?:[^\0][^\0])*)\0\0" res))
                                    (let ([len (read-int 4 res)])
                                      (read-bytes len res)))))})
               (make-hash (map (curryr call-with-input-bytes read-record) (regexp-match* #px#"8BIM(.(?!8BIM))+." (current-input-port))))})
    null))

(define psd->bitmap : (-> PSD (Instance Bitmap%))
  (lambda [self]
    (define maybe-bmp : (U (Instance Bitmap%) Special-Comment) (psd-section-image self))
    (cond [(not (special-comment? maybe-bmp)) maybe-bmp]
          [else (let ([image-data : Bytes (assert (special-comment-value maybe-bmp) bytes?)])
                  (define bmp : (Instance Bitmap%)
                    (psd-image-data->bitmap 'psd->bitmap image-data (psd-header-color-mode self)
                                            (psd-header-width self) (psd-header-height self)
                                            (psd-header-channels self) (psd-header-depth self)
                                            (psd-section-compression-mode self) (psd-density self)))
                  (set-psd-section-image! self bmp)
                  bmp)])))

(define psd-size : (-> PSD (Values Positive-Index Positive-Index))
  (lambda [self]
    (define density : Positive-Real (psd-density self))
    (values (~size (psd-header-width self) density)
            (~size (psd-header-height self) density))))

#;(define psd-profile : (->* () (Output-Port) Void)
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
