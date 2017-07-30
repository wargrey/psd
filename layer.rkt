#lang typed/racket/base

;;; http://www.adobe.com/devnet-apps/photoshop/fileformatashtml/#50577409_pgfId-1031423

(provide (all-defined-out))

(require "base.rkt")
(require "digitama/image.rkt")
(require "digitama/psd.rkt")
(require "digitama/draw.rkt")
(require "digitama/parser.rkt")
(require "digitama/unsafe/layer.rkt")

(require "digitama/layer.rkt")
(require "digitama/layer/parser.rkt")
(require "digitama/layer/format.rkt")
(require "digitama/layer/bitmap.rkt")

(define psd-layers : (-> PSD [#:resolve? (U (Listof Symbol) Symbol Boolean)] (Listof PSD-Layer-Object))
  (lambda [self #:resolve? [keys #false]]
    (define layers : (Listof PSD-Layer-Object)
      (psd-ref! self Section-layers
                (λ [layer-info]
                  (let*-values ([(count ps-size) (values (parse-int16 layer-info 0) (PSD-Section-special-size self))]
                                [(chstep rle-ssize) (if (fx= ps-size 4) (values 6 2) (values 10 4))]
                                [(density color-mode) (values (PSD-File-density self) (PSD-Header-color-mode self))])
                    (let parse-layer : (Listof PSD-Layer-Object) ([sdrocer : (Listof PSD-Layer-Record) null]
                                                                  [tsilhc : (Listof (Listof (Pairof Fixnum Index))) null]
                                                                  [sesabofni : (Listof PSD-Layer-Infobase) null]
                                                                  [rest : Fixnum (fxabs count)]
                                                                  [idx : Fixnum 2]
                                                                  [image-total : Fixnum 0])
                      (if (fx> rest 0)
                          (let-values ([(record slennahc image-size infobase next-idx) (parse-layer-record layer-info idx ps-size chstep)])
                            (parse-layer (cons record sdrocer) (cons slennahc tsilhc) (cons infobase sesabofni)
                                         (fx- rest 1) next-idx (fx+ image-total image-size)))
                          (let parse-layer-channels ([records : (Listof PSD-Layer-Record) sdrocer]
                                                     [chlist : (Listof (Listof (Pairof Fixnum Index))) tsilhc]
                                                     [infobases : (Listof PSD-Layer-Infobase) sesabofni]
                                                     [end-idx : Fixnum (fx+ idx image-total)]
                                                     [layers : (Listof PSD-Layer-Object) null])
                            (cond [(null? records) layers]
                                  [else (let*-values ([(record slennahc infobase) (values (car records) (car chlist) (car infobases))]
                                                      [(divider-info) (psd-infobase-ref 'psd-layers infobase 'lsct)]
                                                      [(name-info) (psd-infobase-ref 'psd-layers infobase 'luni)]
                                                      [(id-info) (psd-infobase-ref 'psd-layers infobase 'lyid)])
                                          (define make-psd-layer ; PSD-Layer-Constructor
                                            (cond [(not (PSD-Layer-Section-Divider? divider-info)) PSD-Layer]
                                                  [else (case (PSD-Layer-Section-Divider-type divider-info)
                                                          [(1) PSD-Layer:Open]
                                                          [(2) PSD-Layer:Closed]
                                                          [(3) PSD-Layer:Divider]
                                                          [else PSD-Layer])]))
                                          (define name : String
                                            (cond [(not (PSD-Layer-Unicode-Name? name-info)) (PSD-Layer-Record-name record)]
                                                  [else (PSD-Layer-Unicode-Name-data name-info)]))
                                          (define id : (U Index Symbol)
                                            (cond [(not (PSD-Layer-Id? id-info)) (gensym 'psd:layer:)]
                                                  [else (PSD-Layer-Id-data id-info)]))
                                          (define-values (channels previous-end-idx)
                                            (let parse-layer-channel : (Values (Listof PSD-Layer-Channel) Fixnum)
                                              ([slennahc : (Listof (Pairof Fixnum Index)) slennahc]
                                               [previous-end-idx : Fixnum end-idx]
                                               [channels : (Listof PSD-Layer-Channel) null])
                                              (cond [(null? slennahc) (values channels previous-end-idx)]
                                                    [else (let* ([chsize (cdar slennahc)]
                                                                 [chidx (fx- previous-end-idx chsize)]
                                                                 [cmethod (integer->compression-method (parse-uint16 layer-info chidx))]
                                                                 [channel (list (caar slennahc) cmethod (fx+ chidx 2) (fx- chsize 2))])
                                                            (parse-layer-channel (cdr slennahc) chidx
                                                                                 (cons (ann channel PSD-Layer-Channel) channels)))])))
                                          (parse-layer-channels (cdr records) (cdr chlist) (cdr infobases) previous-end-idx
                                                                (cons (make-psd-layer id name channels (fx< count 0)
                                                                                      record infobase layer-info color-mode density)
                                                                      layers)))]))))))))
    (unless (not keys)
      (for ([layer (in-list layers)])
        (psd-layer-infobase layer #:resolve? keys)))
    layers))

(define psd-global-layer-mask : (-> PSD (Option PSD-Global-Layer-Mask))
  (lambda [self]
    (psd-ref! self Section-global-mask
              (λ [mask-info]
                (PSD-Global-Layer-Mask (parse-int16 mask-info 0)
                                       (list (parse-uint16 mask-info 2)
                                             (parse-uint16 mask-info 4)
                                             (parse-uint16 mask-info 6)
                                             (parse-uint16 mask-info 8))
                                       (integer->mask-opacity (parse-int16 mask-info 10 index?))
                                       (integer->mask-kind (parse-uint8 mask-info 12)))))))

(define psd-tagged-blocks : (-> PSD [#:resolve? (U (Listof Symbol) Symbol Boolean)] PSD-Layer-Infobase)
  (lambda [self #:resolve? [keys #true]]
    (define infobase : PSD-Layer-Infobase
      (psd-ref! self Section-tagged-blocks
                (λ [block-info] (let-values ([(infobase end-idx) (parse-tagged-blocks block-info 0 (PSD-Section-special-size self))])
                                  infobase))))
    (unless (not keys)
      (for ([key (cond [(boolean? keys) (in-list (hash-keys infobase))]
                       [(symbol? keys) (in-value keys)]
                       [else (in-list keys)])])
        (psd-infobase-ref 'psd-tagged-block-ref infobase key)))
    infobase))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define psd-layer-location : (-> PSD-Layer-Object (Values Fixnum Fixnum))
  (lambda [self]
    (define record : PSD-Layer-Record (PSD-Layer-Subject-record self))
    (values (PSD-Layer-Header-x record) (PSD-Layer-Header-y record))))

(define psd-layer-size : (-> PSD-Layer-Object (Values Fixnum Fixnum))
  (lambda [self]
    (define record : PSD-Layer-Record (PSD-Layer-Subject-record self))
    (values (PSD-Layer-Header-width record) (PSD-Layer-Header-width record))))

#;(define psd-layer-composite-bitmap : (-> PSD-Layer-Object (Instance Bitmap%))
  (lambda [self]
    (psd-ref! self Layer-Object-image
              (λ [image-data]
                (psd-layer-image-data->bitmap 'psd-layer-composite-bitmap image-data (PSD-Header-color-mode self)
                                              (PSD-Header-width self) (PSD-Header-height self)
                                              (PSD-Header-channels self) (PSD-Header-depth self)
                                              (PSD-Header-compression-method self) (PSD-File-density self))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; http://www.adobe.com/devnet-apps/photoshop/fileformatashtml/#50577409_71546

(define psd-layer-infobase : (-> PSD-Layer-Object [#:resolve? (U (Listof Symbol) Symbol Boolean)] PSD-Layer-Infobase)
  (lambda [self #:resolve? [keys #true]]
    (define infobase : PSD-Layer-Infobase (PSD-Layer-Object-infobase self))
    (unless (not keys)
      (for ([key (cond [(boolean? keys) (in-list (hash-keys infobase))]
                       [(symbol? keys) (in-value keys)]
                       [else (in-list keys)])])
        (psd-infobase-ref 'psd-layer-info-ref infobase key)))
    infobase))

(define psd-layer-info-ref : (-> PSD-Layer-Object Symbol (Option PSD-Layer-Info))
  (lambda [self key]
    (psd-infobase-ref 'psd-layer-info-ref (PSD-Layer-Object-infobase self) key)))

(define psd-layer-resolve-infobase : (->* (PSD-Layer-Object) ((U (Listof Symbol) Symbol)) Void)
  (lambda [self [keys #true]]
    (void (psd-layer-infobase self #:resolve? keys))))

(define psd-tagged-block-ref : (-> PSD Symbol (Option PSD-Layer-Info))
  (lambda [self key]
    (psd-infobase-ref 'psd-tagged-block-ref (psd-tagged-blocks self) key)))

(define psd-resolve-tagged-blocks : (->* (PSD) ((U (Listof Symbol) Symbol)) Void)
  (lambda [self [keys #true]]
    (void (psd-tagged-blocks self #:resolve? keys))))
