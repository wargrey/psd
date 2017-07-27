#lang typed/racket/base

(provide (all-defined-out))

(require "../layer.rkt")
(require "../parser.rkt")
(require "../exn.rkt")
(require "../image.rkt")
(require "format.rkt")

(define parse-layer-record : (-> Bytes Fixnum Positive-Byte Positive-Byte (values PSD-Layer-Record PSD-Layer-Infobase Index Fixnum))
  (lambda [layer-info idx ps-size channel-step]
    (define-values (x y width height) (parse-rectangle layer-info idx))
    (define channel-count : Index (parse-size layer-info (fx+ idx 16) 2))
    (define-values (channels 8BIM-idx image-data-size)
      (let cons-info : (Values (Listof (Pairof Fixnum Index)) Fixnum Fixnum)
        ([channel-No. : Fixnum 0]
         [channel-start : Fixnum (fx+ idx 18)]
         [image-data-size : Fixnum 0]
         [slennahc : (Listof (Pairof Fixnum Index)) null])
        (cond [(fx= channel-No. channel-count) (values (reverse slennahc) channel-start image-data-size)]
              [else (let ([channel-id (parse-int16 layer-info channel-start)]
                          [channel-size (parse-size layer-info (fx+ channel-start 2) ps-size)])
                      (cons-info (fx+ channel-No. 1) (fx+ channel-start channel-step)
                                 (fx+ channel-size image-data-size) (cons (cons channel-id channel-size) slennahc)))])))
    (define signature : Bytes (parse-nbytes layer-info 8BIM-idx 4))
    (unless (equal? signature #"8BIM")
      (raise-user-error 'psd-layers "not an valid layer record: ~a" signature))
    (define blend : PSD-Blend-Mode (parse-keyword layer-info (fx+ 8BIM-idx 4) psd-blend-mode?))
    (define-values (opacity base-clipping? flags #;total:mask+|blending-ranges|+|pascal-name|+infobase)
      (values (parse-uint8 layer-info (fx+ 8BIM-idx 8))
              (fx= (parse-uint8 layer-info (fx+ 8BIM-idx 9)) 0)
              (layer-flags->symbols (parse-uint8 layer-info (fx+ 8BIM-idx 10)))
              #;(parse-size layer-info (fx+ 8BIM-idx 12) 4)))
    (define-values (mask range-idx) (parse-layer-mask layer-info (fx+ 8BIM-idx 16)))
    (define-values (blending-ranges name-idx) (parse-layer-blending-ranges layer-info range-idx channel-count))
    (define-values (pascal-name 8B64-idx) (parse-pascal-string*n layer-info name-idx 4))
    (define-values (infobase end-idx) (parse-tagged-blocks layer-info 8B64-idx ps-size))
    (values (PSD-Layer-Record x y width height channels blend opacity base-clipping? flags mask blending-ranges pascal-name)
            infobase (assert image-data-size index?)
            end-idx #;(fx+ (fx+ 8BIM-idx 16) total:mask+|blending-ranges|+|pascal-name|+infobase))))

(define parse-layer-mask : (-> Bytes Fixnum (Values (Option PSD-Layer-Mask) Fixnum))
  (lambda [layer-info idx]
    (define size : Index (parse-size layer-info idx 4))
    (values (and (fx> size 0)
                 (let-values ([(x y width height) (parse-rectangle layer-info (fx+ idx 4))]
                              [(defcolor) (parse-uint8 layer-info (fx+ idx 20))]
                              [(flags) (parse-uint8 layer-info (fx+ idx 21))]
                              [(mask-idx) (fx+ idx 22)])
                   (define-values (parameter real-idx)
                     (cond [(bitwise-bit-set? flags 4) (parse-mask-parameter layer-info mask-idx)]
                           [else (values psd-layer-mask-default-parameter mask-idx)]))
                   (if (fx= (fx+ (fx+ idx 4) size) (fx+ real-idx 2))
                       (PSD-Layer-Mask x y width height defcolor (mask-flags->symbols flags) parameter)
                       (let-values ([(rx ry rwidth rheight) (parse-rectangle layer-info (fx+ real-idx 2))])
                         (PSD-Layer-Real-Mask x y width height defcolor (mask-flags->symbols flags) parameter
                                              (mask-flags->symbols (parse-uint8 layer-info (fx+ real-idx 0)))
                                              (parse-uint8 layer-info (fx+ real-idx 1))
                                              rx ry rwidth rheight)))))
            (fx+ (fx+ idx 4) size))))

(define parse-layer-blending-ranges : (-> Bytes Fixnum Index (Values PSD-Blending-Ranges Fixnum))
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
            (fx+ (fx+ idx 4) size))))

(define parse-tagged-blocks : (-> Bytes Fixnum Positive-Byte (Values PSD-Layer-Infobase Fixnum))
  (let ()
    (define parse-8B64 : (-> Bytes Fixnum Positive-Byte (Values Symbol PSD-Layer-Segment Fixnum))
      (lambda [src start ps-size]
        (define key : Symbol (parse-keyword src start))
        (define size-idx : Fixnum (fx+ start 4))
        (define ssize : Byte
          (cond [(fx= ps-size 4) ps-size]
                [(memq key '(lmsk lr16 lr32 layr mt16 mt32 mtrn alph fmsk lnk2 feid fxid pxsd)) 8]
                [else 4]))
        (define size : Index (parse-size src size-idx ssize))
        (define raw-idx : Fixnum (fx+ size-idx ssize))
        (values key (vector src raw-idx size) (fx+ raw-idx size))))
    (lambda [layer-info idx ps-size]
      (let parse-8BIM ([start : Fixnum idx]
                       [blocks : (Listof (Pairof Symbol PSD-Layer-Segment)) null])
        (if (regexp-match? #px"^8B(IM|64)" layer-info start)
            (let-values ([(key info next) (parse-8B64 layer-info (fx+ start 4) ps-size)])
              (parse-8BIM next (cons (cons key info) blocks)))
            (values (make-hasheq blocks) start))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define psd-layer-mask-default-parameter : PSD-Layer-Mask-Parameter (vector #false #false #false #false))

(define parse-rectangle : (-> Bytes Fixnum (Values Fixnum Fixnum Index Index))
  (lambda [src idx]
    (define-values (top left bottom right)
      (values (parse-int32 src (fx+ idx 0))
              (parse-int32 src (fx+ idx 4))
              (parse-int32 src (fx+ idx 8))
              (parse-int32 src (fx+ idx 12))))
    (values top left (assert (fx- right left) index?) (assert (fx- bottom top) index?))))

(define parse-mask-parameter : (-> Bytes Fixnum (Values PSD-Layer-Mask-Parameter Fixnum))
  (lambda [src idx]
    (define-values (mask i0) (values (parse-uint8 src idx) (fx+ idx 1)))
    (define-values (udensity i1) (if (bitwise-bit-set? mask 0) (values (parse-uint8 src i0) (fx+ i0 1)) (values #false i0)))
    (define-values (ufeather i2) (if (bitwise-bit-set? mask 1) (values (parse-double src i1) (fx+ i1 8)) (values #false i1)))
    (define-values (vdensity i3) (if (bitwise-bit-set? mask 2) (values (parse-uint8 src i2) (fx+ i2 1)) (values #false i2)))
    (define-values (vfeather i4) (if (bitwise-bit-set? mask 3) (values (parse-double src i3) (fx+ i3 8)) (values #false i3)))
    (values (vector udensity ufeather vdensity vfeather) i4)))

(define parse-blending-range : (-> Bytes Fixnum (Pairof PSD-Blending-Range PSD-Blending-Range))
  (lambda [src idx]
    (define source : PSD-Blending-Range
      (vector (parse-uint8 src (fx+ idx 0))
              (parse-uint8 src (fx+ idx 1))
              (parse-uint8 src (fx+ idx 2))
              (parse-uint8 src (fx+ idx 3))))
    (define dest : PSD-Blending-Range
      (vector (parse-uint8 src (fx+ idx 4))
              (parse-uint8 src (fx+ idx 5))
              (parse-uint8 src (fx+ idx 6))
              (parse-uint8 src (fx+ idx 7))))
    (cons source dest)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
                  (and (bitwise-bit-set? flag 2) 'inverse)
                  (and (bitwise-bit-set? flag 3) 'actual)
                  (and (bitwise-bit-set? flag 4) 'parameterized)))))
