#lang typed/racket/base

(provide (all-defined-out))

(require racket/fixnum)

(require "../layer.rkt")
(require "../parser.rkt")

(define parse-layer-record : (-> Bytes Fixnum Byte Byte (values PSD-Layer Fixnum))
  (lambda [layer-info idx psd/psb-size channel-step]
    (define rectangle : PSD-Layer-Rectangle (parse-rectangle layer-info idx))
    (define channel-count : Index (parse-size layer-info (fx+ idx 16) 2))
    (define-values (channels 8BIM-idx)
      (let cons-info : (Values (Listof (Pairof Fixnum Index)) Fixnum)
        ([channel-No. : Fixnum 0]
         [channel-start : Fixnum (fx+ idx 18)]
         [slennahc : (Listof (Pairof Fixnum Index)) null])
        (cond [(fx= channel-No. channel-count) (values (reverse slennahc) channel-start)]
              [else (cons-info (fx+ channel-No. 1)
                               (fx+ channel-start channel-step)
                               (cons (cons (parse-int16 layer-info channel-start)
                                           (parse-size layer-info (fx+ channel-start 2) psd/psb-size))
                                     slennahc))])))
    (define signature : Bytes (parse-nbytes layer-info 8BIM-idx 4))
    (unless (equal? signature #"8BIM")
      (raise-user-error 'psd-layers "not an valid layer record: ~a" signature))
    (define blend : PSD-Blend-Mode (parse-keyword layer-info (fx+ 8BIM-idx 4) 4 psd-blend-mode?))
    (define-values (opacity clipping flags total)
      (values (parse-uint8 layer-info (fx+ 8BIM-idx 8))
              (parse-uint8 layer-info (fx+ 8BIM-idx 9))
              (parse-uint8 layer-info (fx+ 8BIM-idx 10))
              (parse-size layer-info (fx+ 8BIM-idx 12) 4)))
    (define-values (mask mask-size) (parse-layer-mask layer-info (fx+ 8BIM-idx 16)))
    (define-values (blending-ranges ranges-size) (parse-layer-blending-ranges layer-info (fx+ (fx+ 8BIM-idx 20) mask-size) channel-count))
    (define-values (name name-size) (parse-pascal-string layer-info (fx+ (fx+ 8BIM-idx 24) (fx+ mask-size ranges-size))))
    (values (PSD-Layer name rectangle channels blend opacity (fx= clipping 0) (layer-flags->symbols flags) mask blending-ranges)
            (fx+ (fx+ 8BIM-idx 16) total))))

(define parse-layer-mask : (-> Bytes Fixnum (Values (Option PSD-Layer-Mask) Index))
  (lambda [layer-info idx]
    (define size : Index (parse-size layer-info idx 4))
    (values (and (fx> size 0)
                 (let ([rectangle (parse-rectangle layer-info (fx+ idx 4))]
                       [defcolor (parse-uint8 layer-info (fx+ idx 20))]
                       [flags (parse-uint8 layer-info (fx+ idx 21))])
                   (cond [(fx= size 20) (PSD-Layer-Mask rectangle defcolor (mask-flags->symbols flags))]
                         [else (let ([mask (parse-uint8 layer-info (fx+ idx 22))])
                                 (PSD-Layer-Mask rectangle defcolor (mask-flags->symbols flags)))])))
            size)))

(define parse-layer-blending-ranges : (-> Bytes Fixnum Index (Values PSD-Blending-Ranges Index))
  (lambda [layer-info idx count]
    (define size : Index (parse-size layer-info idx 4))
    (values (cons (parse-blending-range layer-info (fx+ idx 4))
                  (let parse-channel-ranges : (Listof (Pairof PSD-Blending-Range PSD-Blending-Range))
                    ([rest : Fixnum count]
                     [range-idx : Fixnum (fx+ idx 12)]
                     [segnar : (Listof (Pairof PSD-Blending-Range PSD-Blending-Range)) null])
                    (cond [(fx= rest 0) (reverse segnar)]
                          [else (parse-channel-ranges (fx- rest 1) (fx+ range-idx 8)
                                                      (cons (parse-blending-range layer-info range-idx) segnar))])))
            size)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define parse-rectangle : (-> Bytes Fixnum PSD-Layer-Rectangle)
  (lambda [layer-info idx]
    (define-values (top left bottom right)
      (values (parse-int32 layer-info (fx+ idx 0))
              (parse-int32 layer-info (fx+ idx 4))
              (parse-int32 layer-info (fx+ idx 8))
              (parse-int32 layer-info (fx+ idx 12))))
    (vector top left (assert (fx- right left) index?) (assert (fx- bottom top) index?))))

(define parse-blending-range : (-> Bytes Fixnum (Pairof PSD-Blending-Range PSD-Blending-Range))
  (lambda [layer-info idx]
    (define source : PSD-Blending-Range
      (vector (parse-uint8 layer-info (fx+ idx 0))
              (parse-uint8 layer-info (fx+ idx 1))
              (parse-uint8 layer-info (fx+ idx 2))
              (parse-uint8 layer-info (fx+ idx 3))))
    (define dest : PSD-Blending-Range
      (vector (parse-uint8 layer-info (fx+ idx 4))
              (parse-uint8 layer-info (fx+ idx 5))
              (parse-uint8 layer-info (fx+ idx 6))
              (parse-uint8 layer-info (fx+ idx 7))))
    (cons source dest)))

(define layer-flags->symbols : (-> Byte (Listof Symbol))
  (lambda [flag]
    (filter symbol?
            (list (and (bitwise-bit-set? flag 0) 'transparency-protected)
                  (and (bitwise-bit-set? flag 1) 'invisible)
                  (and (bitwise-bit-set? flag 2) 'obsolete)
                  (and (bitwise-bit-set? flag 3) (bitwise-bit-set? flag 4) 'irrelevant)))))

(define mask-flags->symbols : (-> Byte (Listof Symbol))
  (lambda [flag]
    (filter symbol?
            (list (and (bitwise-bit-set? flag 0) 'relative)
                  (and (bitwise-bit-set? flag 1) 'disabled)
                  (and (bitwise-bit-set? flag 2) 'inversive)
                  (and (bitwise-bit-set? flag 3) 'actual)
                  (and (bitwise-bit-set? flag 4) 'parameterized)))))
