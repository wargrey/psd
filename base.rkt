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

(require "digitama/resource.rkt")
(require "digitama/resources/format.rkt")
(require "digitama/unsafe/resource.rkt")

(struct PSD PSD-Section ([density : Positive-Real])
  #:transparent
  #:constructor-name use-read-psd-instead
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
          (use-read-psd-instead (integer->version version) channels width height depth color-mode 
                                (make-special-comment color-mode-data) (make-special-comment image-resources)
                                (if layer-info (make-special-comment layer-info) null)
                                (and mask-info (make-special-comment mask-info))
                                (if tagged-blocks (make-special-comment tagged-blocks) (make-hasheq))
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
;;; http://www.adobe.com/devnet-apps/photoshop/fileformatashtml/#50577409_38034
  
(define psd-resources : (-> PSD [#:resolve? (U (Listof Integer) Integer Boolean)] PSD-Image-Resources)
  (lambda [self #:resolve? [ids #true]]
    (define resources : PSD-Image-Resources
      (psd-ref! self Section-resources
                (λ [resource-data]
                  (let parse-8BIM : PSD-Image-Resources ([start : Fixnum 0]
                                                         [blocks : (Listof (Pairof Fixnum PSD-Image-Resource-Segment)) null])
                    (if (regexp-match? #px"^8BIM" resource-data start)
                        (let ([id (parse-int16 resource-data (fx+ start 4))])
                          (define-values (pascal size-idx) (parse-pascal-string*n resource-data (fx+ start 6) 2))
                          (define segsize : Index (parse-size resource-data size-idx 4))
                          (define segstart : Fixnum (fx+ size-idx 4))
                          (define block : PSD-Image-Resource-Segment (vector pascal (make-special-comment resource-data) segstart segsize))
                          (parse-8BIM (fx+ (fx+ segstart segsize) (fxremainder segsize 2)) (cons (cons id block) blocks)))
                        (make-hasheq blocks))))))
    (unless (not ids)
      (for ([id (cond [(list? ids) (in-list ids)]
                      [(integer? ids) (in-value ids)]
                      [else (in-list (hash-keys resources))])])
        (define maybe-res : (U PSD-Image-Resource-Segment PSD-Resource Void) (hash-ref resources id void))
        (when (vector? maybe-res)
          (define-values (name start size) (values (vector-ref maybe-res 0) (vector-ref maybe-res 2) (vector-ref maybe-res 3)))
          (define block : Bytes (psd-unbox (vector-ref maybe-res 1)))
          (psd-resource-parse!
           resources id /psd/resources
           (λ [[parse : PSD-Resource-Parser]]
             (case id
               [(#x040C) (parse id name block start size (list (PSD-density self)))]
               [(#x03EF) (parse id name block start size (list (hash-has-key? resources #x0435)))]
               [else (parse id name block start size null)]))
           (λ [] (throw-unsupported-error 'psd-resource-ref "unimplemeneted resource: ~a" (psd-id->string id)))
           psd-warn-broken-resource))))
    resources))

(define psd-resource-ref : (-> PSD Integer (Option PSD-Resource))
  (lambda [self id]
    (define resources : PSD-Image-Resources (psd-resources self #:resolve? id))
    (define maybe-res : (U PSD-Image-Resource-Segment PSD-Resource Void) (hash-ref resources id void))
    (and (PSD-Resource? maybe-res) maybe-res)))

(define psd-resolve-resources : (->* (PSD) ((U (Listof Integer) Integer)) Void)
  (lambda [self [ids #true]]
    (void (psd-resources self #:resolve? ids))))

(define psd-grid+guides : (-> PSD (Option PSD-Grid+Guides)) (λ [self] (psd-assert (psd-resource-ref self #x408) PSD-Grid+Guides?)))
(define psd-thumbnail : (-> PSD (Option PSD-Thumbnail)) (λ [self] (psd-assert (psd-resource-ref self #x40C) PSD-Thumbnail?)))
(define psd-file-info : (-> PSD (Option PSD-File-Info)) (λ [self] (psd-assert (psd-resource-ref self #x424) PSD-File-Info?)))