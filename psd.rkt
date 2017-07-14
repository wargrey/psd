#lang typed/racket/base

;;; http://www.adobe.com/devnet-apps/photoshop/fileformatashtml

(provide (all-defined-out))

(require "digitama/psd.rkt")
(require "digitama/draw.rkt")
(require "digitama/stdin.rkt")
(require "digitama/bitmap.rkt")
(require "digitama/parser.rkt")
(require "digitama/resources/format.rkt")
(require "digitama/unsafe/resource.rkt")
(require "digitama/misc.rkt")

(struct: psd : PSD psd-section ([density : Positive-Real])
  #:constructor-name use-read-psd-instead
  #:property prop:convertible
  (λ [[self : PSD] [request : Symbol] [default : Any]]
    (define maybe-bmp : (Option (Instance Bitmap%))
      (with-handlers ([exn? (λ [[e : exn]] (psd->thumbnail self))])
        (psd->bitmap self)))
    (cond [(not maybe-bmp) default]
          [else (convert maybe-bmp request default)])))

(define read-psd : (->* ((U Path-String Input-Port)) (#:backing-scale Positive-Real #:try-@2x? Boolean) PSD)
  (lambda [/dev/psdin #:backing-scale [density 1.0] #:try-@2x? [try-@2x? #false]]
    (if (input-port? /dev/psdin)
        (let*-values ([(version channels height width depth color-mode) (read-psd-header /dev/psdin)]
                      [(color-mode-data image-resources layer+mask compression-mode image-data) (read-psd-section /dev/psdin version)])
          (use-read-psd-instead (integer->version version) channels width height depth color-mode 
                                (make-special-comment color-mode-data)
                                (make-special-comment image-resources)
                                (make-special-comment layer+mask)
                                compression-mode (make-special-comment image-data) density))
        (let-values ([(path scale) (select-psd-file /dev/psdin density try-@2x?)])
          (call-with-input-file* path (λ [[psdin : Input-Port]] (read-psd psdin #:backing-scale scale)))))))

(define read-psd-as-bitmap : (->* ((U Path-String Input-Port)) (#:backing-scale Positive-Real #:try-@2x? Boolean) (Instance Bitmap%))
  (lambda [/dev/psdin #:backing-scale [density 1.0] #:try-@2x? [try-@2x? #false]]
    (psd->bitmap (read-psd /dev/psdin #:backing-scale density #:try-@2x? try-@2x?))))

(define psd-resources : (-> PSD PSD-Image-Resources)
  (lambda [self]
    (psd-ref! self section-resources
              (λ [resource-data]
                (let parse-8BIM : PSD-Image-Resources ([start : Integer 0]
                                                       [blocks : (Listof (Pairof Integer PSD-Resource-Block)) null])
                  (cond [(and (regexp-match? #px"^8BIM" resource-data start) (index? start))
                         (define id : Integer (parse-integer resource-data 2 #false (fx+ start 4)))
                         (define-values (pascal psize) (parse-pascal-string resource-data (fx+ start 6)))
                         (define size-start : Integer (fx+ (fx+ start 8) (fx+ psize (fxremainder psize 2))))
                         (define data-size : Integer (parse-integer resource-data 4 #false size-start))
                         (define data-start : Integer (fx+ size-start 4))
                         (define raw : Bytes (parse-nbytes resource-data data-size data-start))
                         (parse-8BIM (fx+ (fx+ data-start data-size) (fxremainder data-size 2))
                                     (cons (cons id (cons pascal (make-special-comment raw))) blocks))]
                        [(null? blocks) psd-empty-resources]
                        [else (make-hasheq blocks)]))))))

(define psd-resource-ref : (-> PSD Integer (Option PSD-Resource))
  ;;; http://www.adobe.com/devnet-apps/photoshop/fileformatashtml/#50577409_38034
  (lambda [self id]
    (define resources : PSD-Image-Resources (psd-resources self))
    (define maybe-res (hash-ref resources id void))
    (or (and (psd-resource? maybe-res) maybe-res)
        (and (pair? maybe-res)
             (let ([res.rkt (collection-file-path (format "id~a.rkt" id) "psd" "digitama" "resources")]
                   [main (string->symbol (format "0x~x" id))])
               (define make-resource (with-handlers ([exn? void]) (dynamic-require res.rkt main)))
               (if (psd-resource-constructor? make-resource)
                   (let ([block (psd-unbox (cdr maybe-res))]
                         [name (car maybe-res)])
                     (define res : PSD-Resource
                       (case id
                         [(#x40C) (make-resource id block name (list (psd-density self)))]
                         [else (make-resource id block name null)]))
                     (hash-set! resources id res)
                     res)
                   (throw-unsupported-error 'psd-resource-ref "unimplemeneted resource: 0x~X" id)))))))

(define psd-resolve-resources : (->* (PSD) ((Listof Integer)) Void)
  (lambda [self [ids null]]
    (for ([id (in-list (if (null? ids) (hash-keys (psd-resources self)) ids))])
      (with-handlers ([exn? void])
        (psd-resource-ref self id)))))

(define psd->bitmap : (-> PSD (Instance Bitmap%))
  (lambda [self]
    (psd-ref! self section-image
              (λ [image-data]
                (psd-image-data->bitmap 'psd->bitmap image-data (psd-header-color-mode self)
                                        (psd-header-width self) (psd-header-height self)
                                        (psd-header-channels self) (psd-header-depth self)
                                        (psd-section-compression-mode self) (psd-density self))))))

(define psd->thumbnail : (-> PSD (Option (Instance Bitmap%)))
  (lambda [self]
    (define maybe-preview : (Option PSD-Thumbnail) (psd-thumbnail self))
    (and maybe-preview (psd-thumbnail-image maybe-preview))))

(define psd-size : (-> PSD (Values Positive-Index Positive-Index))
  (lambda [self]
    (define density : Positive-Real (psd-density self))
    (values (~size (psd-header-width self) density)
            (~size (psd-header-height self) density))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define psd-grid+guides : (-> PSD (Option PSD-Grid+Guides)) (λ [self] (psd-assert (psd-resource-ref self #x408) psd-grid+guides?)))
(define psd-thumbnail : (-> PSD (Option PSD-Thumbnail)) (λ [self] (psd-assert (psd-resource-ref self #x40C) psd-thumbnail?)))
(define psd-file-info : (-> PSD (Option PSD-File-Info)) (λ [self] (psd-assert (psd-resource-ref self #x424) psd-file-info?)))

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
