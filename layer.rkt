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

(define psd-layers : (-> PSD [#:resolve? (U (Listof Symbol) Symbol Boolean)] (Listof PSD-Layer-Record))
  (lambda [self #:resolve? [keys #false]]
    (define layers : (Listof PSD-Layer-Record)
      (psd-ref! self Section-layers
                (λ [layer-info]
                  (let ([count : Fixnum (parse-int16 layer-info 0)])
                    (define-values (psd/psb-size channel-step) (if (psd-large-document? self) (values 8 10) (values 4 6)))
                    (let parse-layer : (Listof PSD-Layer-Record) ([sreyal : (Listof PSD-Layer-Record) null]
                                                                  [rest : Fixnum (fxabs count)]
                                                                  [idx : Fixnum 2])
                      (if (fx> rest 0)
                          (let-values ([(layer next-idx) (parse-layer-record layer-info idx psd/psb-size channel-step)])
                            (parse-layer (cons layer sreyal) (fx- rest 1) next-idx))
                          (reverse sreyal)))))))
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
                (λ [block-info]
                  (let ([psd/psb-size : Byte (if (psd-large-document? self) 8 4)])
                    (parse-tagged-blocks block-info 0 psd/psb-size)))))
    (unless (not keys)
      (for ([key (cond [(boolean? keys) (in-list (hash-keys infobase))]
                       [(symbol? keys) (in-value keys)]
                       [else (in-list keys)])])
        (psd-infobase-ref 'psd-tagged-block-ref infobase key)))
    infobase))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; http://www.adobe.com/devnet-apps/photoshop/fileformatashtml/#50577409_71546

(define psd-layer-infobase : (-> PSD-Layer-Record [#:resolve? (U (Listof Symbol) Symbol Boolean)] PSD-Layer-Infobase)
  (lambda [self #:resolve? [keys #true]]
    (define infobase : PSD-Layer-Infobase (PSD-Layer-Record-infobase self))
    (unless (not keys)
      (for ([key (cond [(boolean? keys) (in-list (hash-keys infobase))]
                       [(symbol? keys) (in-value keys)]
                       [else (in-list keys)])])
        (psd-infobase-ref 'psd-layer-info-ref infobase key)))
    infobase))

(define psd-layer-info-ref : (-> PSD-Layer-Record Symbol (Option PSD-Layer-Info))
  (lambda [self key]
    (psd-infobase-ref 'psd-layer-info-ref (PSD-Layer-Record-infobase self) key)))

(define psd-layer-resolve-infobase : (->* (PSD-Layer-Record) ((U (Listof Symbol) Symbol)) Void)
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
    (define maybe-info : (U PSD-Layer-Info PSD-Layer-Segment Void False) (and infobase (hash-ref infobase key void)))
    (or (and (PSD-Layer-Info? maybe-info) maybe-info)
        (and (vector? maybe-info)
             (let-values ([(block start size) (values (vector-ref maybe-info 0) (vector-ref maybe-info 1) (vector-ref maybe-info 2))])
               (psd-layer-info-parse!
                infobase key /psd/layer/blocks
                (λ [[parse : PSD-Layer-Info-Parser]] (parse (psd-unbox block) start size null))
                (λ [] (throw-unsupported-error src "unimplemeneted tagged information: ~a" key))
                psd-warn-broken-information))))))
