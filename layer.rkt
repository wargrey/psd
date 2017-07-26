#lang typed/racket/base

;;; http://www.adobe.com/devnet-apps/photoshop/fileformatashtml/#50577409_pgfId-1031423

(provide (except-out (all-defined-out) psd-infobase-ref))

(require "base.rkt")
(require "digitama/psd.rkt")
(require "digitama/draw.rkt")
(require "digitama/bitmap.rkt")
(require "digitama/parser.rkt")
(require "digitama/misc.rkt")
(require "digitama/exn.rkt")
(require "digitama/unsafe/layer.rkt")

(require "digitama/layer.rkt")
(require "digitama/layer/parser.rkt")
(require "digitama/layer/format.rkt")

(require "digitama/layer/blocks/lsct.rkt")

(define psd-layers : (-> PSD [#:resolve? (U (Listof Symbol) Symbol Boolean)] (Listof PSD-Layer-Object))
  (lambda [self #:resolve? [keys #false]]
    (define layers : (Listof PSD-Layer-Object)
      (psd-ref! self Section-layers
                (λ [layer-info]
                  (let* ([count : Fixnum (parse-int16 layer-info 0)]
                         [ps-size : Positive-Byte (PSD-special-size self)]
                         [channel-step : Positive-Byte (if (fx= ps-size 4) 6 10)])
                    (let parse-layer : (Listof PSD-Layer-Object) ([sdrocer : (Listof PSD-Layer-Record) null]
                                                                  [sesabofni : (Listof PSD-Layer-Infobase) null]
                                                                  [rest : Fixnum (fxabs count)]
                                                                  [idx : Fixnum 2])
                      (if (fx> rest 0)
                          (let-values ([(record infobase next-idx) (parse-layer-record layer-info idx ps-size channel-step)])
                            (parse-layer (cons record sdrocer) (cons infobase sesabofni) (fx- rest 1) next-idx))
                          (let parse-layer-images : (Listof PSD-Layer-Object) ([records : (Listof PSD-Layer-Record) sdrocer]
                                                                               [infobases : (Listof PSD-Layer-Infobase) sesabofni]
                                                                               [idx : Fixnum idx]
                                                                               [layers : (Listof PSD-Layer-Object) null])
                            (cond [(null? records) layers]
                                  [else (let-values ([(record infobase) (values (car records) (car infobases))])
                                          (define make-psd-layer ; PSD-Layer-Constructor
                                            (let ([divider (psd-infobase-ref 'psd-layers infobase 'lsct)])
                                              (cond [(not (PSD-Layer-Section-Divider? divider)) PSD-Layer]
                                                    [else (case (PSD-Layer-Section-Divider-type divider)
                                                            [(1) PSD-Layer:Open] [(2) PSD-Layer:Closed] [(3) PSD-Layer:Divider]
                                                            [else PSD-Layer])])))
                                          (parse-layer-images (cdr records) (cdr infobases) idx
                                                              (cons (make-psd-layer 1 "" record infobase) layers)))]))))))))
    (unless (not keys)
      (for ([layer (in-list layers)])
        (psd-layer-infobase layer #:resolve? keys)))
    layers))

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

(define psd-tagged-blocks : (-> PSD [#:resolve? (U (Listof Symbol) Symbol Boolean)] PSD-Layer-Infobase)
  (lambda [self #:resolve? [keys #true]]
    (define infobase : PSD-Layer-Infobase
      (psd-ref! self Section-tagged-blocks
                (λ [block-info] (let-values ([(infobase end-idx) (parse-tagged-blocks block-info 0 (PSD-special-size self))])
                                  infobase))))
    (unless (not keys)
      (for ([key (cond [(boolean? keys) (in-list (hash-keys infobase))]
                       [(symbol? keys) (in-value keys)]
                       [else (in-list keys)])])
        (psd-infobase-ref 'psd-tagged-block-ref infobase key)))
    infobase))

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

(define psd-infobase-ref : (-> Symbol PSD-Layer-Infobase Symbol (Option PSD-Layer-Info))
  (lambda [src infobase key]
    (define maybe-info : (U PSD-Layer-Info PSD-Layer-Segment Void) (hash-ref infobase key void))
    (or (and (PSD-Layer-Info? maybe-info) maybe-info)
        (and (vector? maybe-info)
             (let-values ([(block start size) (values (vector-ref maybe-info 0) (vector-ref maybe-info 1) (vector-ref maybe-info 2))])
               (psd-layer-info-parse!
                infobase key /psd/layer/blocks
                (λ [[parse : PSD-Layer-Info-Parser]] (parse (psd-unbox block) start size null))
                (λ [] (throw-unsupported-error src "unimplemeneted tagged information: ~a" key))
                psd-warn-broken-information))))))
