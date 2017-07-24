#lang typed/racket/base

;;; http://www.adobe.com/devnet-apps/photoshop/fileformatashtml/#50577409_pgfId-1031423

(provide (all-defined-out))

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

(define psd-remove-broken-layer-info? : (Parameterof Boolean) (make-parameter #false))
(define psd-remove-unknown-layer-info? : (Parameterof Boolean) (make-parameter #false))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define psd-layers : (-> PSD (Listof PSD-Layer-Record))
  (lambda [self]
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

(define psd-tagged-blocks : (-> PSD (Option PSD-Layer-Blocks))
  (lambda [self]
    (psd-ref! self Section-tagged-blocks
              (λ [block-info]
                (let ([psd/psb-size : Byte (if (psd-large-document? self) 8 4)])
                  (parse-tagged-blocks block-info 0 psd/psb-size))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define psd-layers* : (-> PSD (Listof PSD-Layer-Record))
  (lambda [self]
    (define layers : (Listof PSD-Layer-Record) (psd-layers self))
    (for-each psd-layer-resolve-blocks layers)
    layers))

(define psd-layer-infobase* : (-> PSD-Layer-Record (Option PSD-Layer-Blocks))
  (lambda [self]
    (psd-layer-resolve-blocks self)
    (PSD-Layer-Record-blocks self)))

(define psd-layer-info-ref : (-> PSD-Layer-Record Symbol (Option PSD-Layer-Block))
  ;;; http://www.adobe.com/devnet-apps/photoshop/fileformatashtml/#50577409_71546
  (lambda [self key]
    (define infobase : PSD-Layer-Blocks (PSD-Layer-Record-blocks self))
    (define maybe-info : (U PSD-Layer-Block PSD-Layer-Segment Void) (hash-ref infobase key void))
    (or (and (PSD-Layer-Block? maybe-info) maybe-info)
        (and (vector? maybe-info)
             (psd-parse-layer-block
              key
              (λ [[parse : PSD-Layer-Block-Parser]]
                (define-values (block start size) (values (vector-ref maybe-info 0) (vector-ref maybe-info 1) (vector-ref maybe-info 2)))
                (define info : (Option PSD-Layer-Block)
                  (with-handlers ([exn:fail? psd-warn-broken-information])
                    (parse (psd-unbox block) start size null)))
                (cond [(PSD-Layer-Block? info) (hash-set! infobase key info)]
                      [(psd-remove-broken-layer-info?) (hash-remove! infobase info)])
                info)
              (λ []
                (when (psd-remove-unknown-layer-info?)
                  (hash-remove! infobase key))
                (throw-unsupported-error 'psd-layer-info-ref "unimplemeneted layer information: ~a" key)))))))

(define psd-layer-resolve-blocks : (->* (PSD-Layer-Record) ((Listof Symbol)) Void)
  (lambda [self [keys null]]
    (define infobase : PSD-Layer-Blocks (PSD-Layer-Record-blocks self))
    (for ([key (in-list (if (null? keys) (hash-keys infobase) keys))])
      (with-handlers ([exn? psd-warn-broken-information])
        (psd-layer-info-ref self key)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define psd-tagged-blocks* : (-> PSD (Option PSD-Layer-Blocks))
  (lambda [self]
    (psd-resolve-tagged-blocks self)
    (psd-tagged-blocks self)))

(define psd-tagged-block-ref : (-> PSD Symbol (Option PSD-Layer-Block))
  ;;; http://www.adobe.com/devnet-apps/photoshop/fileformatashtml/#50577409_71546
  (lambda [self key]
    (define maybe-infobase : (Option PSD-Layer-Blocks) (psd-tagged-blocks self))
    (define maybe-info : (U PSD-Layer-Block PSD-Layer-Segment Void False) (and maybe-infobase (hash-ref maybe-infobase key void)))
    (or (and (PSD-Layer-Block? maybe-info) maybe-info)
        (and (vector? maybe-info)
             maybe-infobase
             (psd-parse-layer-block
              key
              (λ [[parse : PSD-Layer-Block-Parser]]
                (define-values (block start size) (values (vector-ref maybe-info 0) (vector-ref maybe-info 1) (vector-ref maybe-info 2)))
                (define info : (Option PSD-Layer-Block)
                  (with-handlers ([exn:fail? psd-warn-broken-information])
                    (parse (psd-unbox block) start size null)))
                (cond [(PSD-Layer-Block? info) (hash-set! maybe-infobase key info)]
                      [(psd-remove-broken-layer-info?) (hash-remove! maybe-infobase info)])
                info)
              (λ []
                (when (psd-remove-unknown-layer-info?)
                  (hash-remove! maybe-infobase key))
                (throw-unsupported-error 'psd-tagged-block-ref "unimplemeneted tagged information: ~a" key)))))))

(define psd-resolve-tagged-blocks : (->* (PSD) ((Listof Symbol)) Void)
  (lambda [self [keys null]]
    (define maybe-infobase : (Option PSD-Layer-Blocks) (psd-tagged-blocks self))
    (unless (not maybe-infobase)
      (for ([key (in-list (if (null? keys) (hash-keys maybe-infobase) keys))])
        (with-handlers ([exn? psd-warn-broken-information])
          (psd-tagged-block-ref self key))))))
