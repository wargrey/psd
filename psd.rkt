#lang typed/racket/base

;;; http://www.adobe.com/devnet-apps/photoshop/fileformatashtml

(provide (all-defined-out))

(require "digitama/psd.rkt")
(require "digitama/draw.rkt")
(require "digitama/stdin.rkt")
(require "digitama/bitmap.rkt")
(require "digitama/parser.rkt")
(require "digitama/misc.rkt")
(require "digitama/exn.rkt")

(require "digitama/layer.rkt")
(require "digitama/layer/record.rkt")

(require "digitama/resource.rkt")
(require "digitama/resources/format.rkt")
(require "digitama/unsafe/resource.rkt")

(define psd-remove-broken-resource? : (Parameterof Boolean) (make-parameter #false))
(define psd-remove-unknown-resource? : (Parameterof Boolean) (make-parameter #true))

(struct PSD PSD-Section ([density : Positive-Real])
  #:transparent
  #:constructor-name make-psd/but-read-psd-should-be-used-instead
  #:property prop:convertible
  (λ [[self : PSD] [request : Symbol] [default : Any]]
    (define maybe-bmp : (Option (Instance Bitmap%))
      (with-handlers ([exn? (λ [[e : exn]] (psd-thumbnail-bitmap self))])
        (psd-composite-bitmap self)))
    (cond [(not maybe-bmp) default]
          [else (convert maybe-bmp request default)])))

(define read-psd : (->* ((U Path-String Input-Port)) (#:backing-scale Positive-Real #:try-@2x? Boolean) PSD)
  (lambda [/dev/psdin #:backing-scale [density 1.0] #:try-@2x? [try-@2x? #false]]
    (if (input-port? /dev/psdin)
        (let*-values ([(version channels height width depth color-mode) (read-psd-header /dev/psdin)]
                      [(color-mode-data image-resources layer-info mask-info tagged-blocks) (read-psd-subsection /dev/psdin version)]
                      [(compression-mode image-data) (read-psd-composite-image /dev/psdin)])
          (make-psd/but-read-psd-should-be-used-instead
           (integer->version version) channels width height depth color-mode 
           (make-special-comment color-mode-data) (make-special-comment image-resources)
           (if layer-info (make-special-comment layer-info) null)
           (and mask-info (make-special-comment mask-info))
           (and tagged-blocks (make-special-comment tagged-blocks))
           compression-mode (make-special-comment image-data) density))
        (let-values ([(path scale) (select-psd-file /dev/psdin density try-@2x?)])
          (call-with-input-file* path (λ [[psdin : Input-Port]] (read-psd psdin #:backing-scale scale)))))))

(define read-psd-as-bitmap : (->* ((U Path-String Input-Port)) (#:backing-scale Positive-Real #:try-@2x? Boolean) (Instance Bitmap%))
  (lambda [/dev/psdin #:backing-scale [density 1.0] #:try-@2x? [try-@2x? #false]]
    (psd-composite-bitmap (read-psd /dev/psdin #:backing-scale density #:try-@2x? try-@2x?))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define psd-large-document? : (-> PSD Boolean)
  (lambda [self]
    (eq? (PSD-Header-version self) 'PSB)))

(define psd-size : (-> PSD (Values Positive-Index Positive-Index))
  (lambda [self]
    (define density : Positive-Real (PSD-density self))
    (values (~size (PSD-Header-width self) density)
            (~size (PSD-Header-height self) density))))

(define psd-depth : (-> PSD (Values Positive-Byte Positive-Byte))
  (lambda [self]
    (values (PSD-Header-depth self)
            (PSD-Header-channels self))))

(define psd-color-mode : (-> PSD PSD-Color-Mode)
  (lambda [self]
    (PSD-Header-color-mode self)))

(define psd-composite-bitmap : (-> PSD (Instance Bitmap%))
  (lambda [self]
    (psd-ref! self Section-image
              (λ [image-data]
                (psd-image-data->bitmap 'psd->bitmap image-data (PSD-Header-color-mode self)
                                        (PSD-Header-width self) (PSD-Header-height self)
                                        (PSD-Header-channels self) (PSD-Header-depth self)
                                        (PSD-Section-compression-mode self) (PSD-density self))))))

(define psd-thumbnail-bitmap : (-> PSD (Option (Instance Bitmap%)))
  (lambda [self]
    (define maybe-preview : (Option PSD-Thumbnail) (psd-thumbnail self))
    (and maybe-preview (PSD-Thumbnail-image maybe-preview))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define psd-resources : (-> PSD PSD-Image-Resources)
  (lambda [self]
    (psd-ref! self Section-resources
              (λ [resource-data]
                (let parse-8BIM : PSD-Image-Resources ([start : Fixnum 0]
                                                       [blocks : (Listof (Pairof Fixnum PSD-Resource-Block)) null])
                  (cond [(and (regexp-match? #px"^8BIM" resource-data start) (index? start))
                         (define id : Fixnum (parse-int16 resource-data (fx+ start 4)))
                         (define-values (pascal psize) (parse-pascal-string resource-data (fx+ start 6)))
                         (define size-start : Fixnum (fx+ (fx+ start 8) (fx+ psize (fxremainder psize 2))))
                         (define data-size : Index (parse-size resource-data size-start 4))
                         (define data-start : Fixnum (fx+ size-start 4))
                         (define raw : Bytes (parse-nbytes resource-data data-start data-size))
                         (parse-8BIM (fx+ (fx+ data-start data-size) (fxremainder data-size 2))
                                     (cons (cons id (cons pascal (make-special-comment raw))) blocks))]
                        [(null? blocks) psd-empty-resources]
                        [else (make-hasheq blocks)]))))))

(define psd-resource-ref : (-> PSD Integer (Option PSD-Resource))
  ;;; http://www.adobe.com/devnet-apps/photoshop/fileformatashtml/#50577409_38034
  (lambda [self id]
    (define resources : PSD-Image-Resources (psd-resources self))
    (define maybe-res (hash-ref resources id void))
    (or (and (PSD-Resource? maybe-res) maybe-res)
        (and (pair? maybe-res)
             (psd-parse-resource
              id
              (λ [[parse : PSD-Resource-Parser]]
                (define-values (name block) (values (car maybe-res) (psd-unbox (cdr maybe-res))))
                (define resource : (Option PSD-Resource)
                  (with-handlers ([exn:fail? psd-warn-broken-resource])
                    (case id
                      [(#x040C) (parse id block name (list (PSD-density self)))]
                      [(#x03EF) (parse id block name (list (hash-has-key? resources #x0435)))]
                      [else (parse id block name null)])))
                (cond [(PSD-Resource? resource) (hash-set! resources id resource)]
                      [(psd-remove-broken-resource?) (hash-remove! resources id)])
                resource)
              (λ []
                (when (psd-remove-unknown-resource?)
                  (hash-remove! resources id))
                (throw-unsupported-error 'psd-resource-ref "unimplemeneted resource: ~a" (psd-id->string id))))))))

(define psd-resolve-resources : (->* (PSD) ((Listof Integer)) Void)
  (lambda [self [ids null]]
    (for ([id (in-list (if (null? ids) (hash-keys (psd-resources self)) ids))])
      (with-handlers ([exn? psd-warn-broken-resource])
        (psd-resource-ref self id)))))

(define psd-grid+guides : (-> PSD (Option PSD-Grid+Guides)) (λ [self] (psd-assert (psd-resource-ref self #x408) PSD-Grid+Guides?)))
(define psd-thumbnail : (-> PSD (Option PSD-Thumbnail)) (λ [self] (psd-assert (psd-resource-ref self #x40C) PSD-Thumbnail?)))
(define psd-file-info : (-> PSD (Option PSD-File-Info)) (λ [self] (psd-assert (psd-resource-ref self #x424) PSD-File-Info?)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define psd-layers : (-> PSD (Listof PSD-Layer))
  (lambda [self]
    (psd-ref! self Section-layers
              (λ [layer-info]
                (let ([count : Fixnum (parse-int16 layer-info 0)])
                  (define-values (psd/psb-size channel-step) (if (psd-large-document? self) (values 8 10) (values 4 6)))
                  (let parse-layer : (Listof PSD-Layer) ([sreyal : (Listof PSD-Layer) null]
                                                        [rest : Fixnum (fxabs count)]
                                                        [idx : Fixnum 2])
                    (if (fx> rest 0)
                        (let-values ([(layer next-idx) (parse-layer-record layer-info idx psd/psb-size channel-step)])
                          (parse-layer (cons layer sreyal) (fx- rest 1) next-idx))
                        (reverse sreyal))))))))

(define psd-global-layer-mask : (-> PSD (Option PSD-Global-Mask))
  (lambda [self]
    (psd-ref! self Section-global-mask
              (λ [mask-info]
                (PSD-Global-Mask (parse-int16 mask-info 0)
                                 (list (parse-uint16 mask-info 2)
                                       (parse-uint16 mask-info 4)
                                       (parse-uint16 mask-info 6)
                                       (parse-uint16 mask-info 8))
                                 (integer->mask-opacity (parse-int16 mask-info 10 index?))
                                 (integer->mask-kind (parse-uint8 mask-info 12)))))))

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
