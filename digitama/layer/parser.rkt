#lang typed/racket/base

(provide (all-defined-out))

(require "../layer.rkt")
(require "../parser.rkt")

(require "format.rkt")
(require "blocks/lsct.rkt")

(define parse-layer-record : (-> Bytes Fixnum Byte Byte (values PSD-Layer-Record Fixnum))
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
    (define blend : PSD-Blend-Mode (parse-keyword layer-info (fx+ 8BIM-idx 4) psd-blend-mode?))
    (define-values (opacity base-clipping? flags total)
      (values (parse-uint8 layer-info (fx+ 8BIM-idx 8))
              (fx= (parse-uint8 layer-info (fx+ 8BIM-idx 9)) 0)
              (layer-flags->symbols (parse-uint8 layer-info (fx+ 8BIM-idx 10)))
              (parse-size layer-info (fx+ 8BIM-idx 12) 4)))
    (define-values (mask range-idx) (parse-layer-mask layer-info (fx+ 8BIM-idx 16)))
    (define-values (blending-ranges name-idx) (parse-layer-blending-ranges layer-info range-idx channel-count))
    (define-values (name 8B64-idx) (parse-pascal-string*n layer-info name-idx 4))
    (define infobase : PSD-Layer-Infobase (parse-tagged-blocks layer-info 8B64-idx psd/psb-size))
    (define divider : PSD-Layer-Section-Divider
      (let ([lsct-info (hash-ref infobase 'lsct void)])
        (cond [(not (vector? lsct-info)) psd-layer-default-type]
              [else (lsct layer-info (vector-ref lsct-info 1) (vector-ref lsct-info 2) null)])))
    (hash-set! infobase 'lsct divider)
    (values (case (PSD-Layer-Section-Divider-type divider)
              [(1) (PSD-Layer:Open name rectangle channels blend opacity base-clipping? flags mask blending-ranges infobase)]
              [(2) (PSD-Layer:Closed name rectangle channels blend opacity base-clipping? flags mask blending-ranges infobase)]
              [(3) (PSD-Layer:Divider name rectangle channels blend opacity base-clipping? flags mask blending-ranges infobase)]
              [else (PSD-Layer name rectangle channels blend opacity base-clipping? flags mask blending-ranges infobase)])
            (fx+ (fx+ 8BIM-idx 16) total))))

(define parse-layer-mask : (-> Bytes Fixnum (Values (Option PSD-Layer-Mask) Fixnum))
  (lambda [layer-info idx]
    (define size : Index (parse-size layer-info idx 4))
    (values (and (fx> size 0)
                 (let ([rectangle (parse-rectangle layer-info (fx+ idx 4))]
                       [defcolor (parse-uint8 layer-info (fx+ idx 20))]
                       [flags (parse-uint8 layer-info (fx+ idx 21))]
                       [mask-idx (fx+ idx 22)])
                   (define-values (parameter real-idx)
                     (cond [(bitwise-bit-set? flags 4) (parse-mask-parameter layer-info mask-idx)]
                           [else (values psd-layer-mask-default-parameter mask-idx)]))
                   (if (fx= (fx+ (fx+ idx 4) size) (fx+ real-idx 2))
                       (PSD-Layer-Mask rectangle defcolor (mask-flags->symbols flags) parameter)
                       (PSD-Layer-Real-Mask rectangle defcolor (mask-flags->symbols flags) parameter
                                            (mask-flags->symbols (parse-uint8 layer-info (fx+ real-idx 0)))
                                            (parse-uint8 layer-info (fx+ real-idx 1))
                                            (parse-rectangle layer-info (fx+ real-idx 2))))))
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

(define parse-tagged-blocks : (-> Bytes Fixnum Byte PSD-Layer-Infobase)
  (lambda [layer-info idx psd/psb-size]
    (let parse-8BIM ([start : Fixnum idx]
                     [blocks : (Listof (Pairof Symbol PSD-Layer-Segment)) null])
      (if (regexp-match? #px"^8B(IM|64)" layer-info start)
          (let-values ([(key info next) (parse-8B64 layer-info start psd/psb-size)])
            (parse-8BIM next (cons (cons key info) blocks)))
          (make-hasheq blocks)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define psd-layer-mask-default-parameter : PSD-Layer-Mask-Parameter (vector #false #false #false #false))
(define psd-layer-default-type : PSD-Layer-Section-Divider (PSD-Layer-Section-Divider 0 #false #false))

(define parse-rectangle : (-> Bytes Fixnum PSD-Layer-Rectangle)
  (lambda [layer-info idx]
    (define-values (top left bottom right)
      (values (parse-int32 layer-info (fx+ idx 0))
              (parse-int32 layer-info (fx+ idx 4))
              (parse-int32 layer-info (fx+ idx 8))
              (parse-int32 layer-info (fx+ idx 12))))
    (vector top left (assert (fx- right left) index?) (assert (fx- bottom top) index?))))

(define parse-mask-parameter : (-> Bytes Fixnum (Values PSD-Layer-Mask-Parameter Fixnum))
  (lambda [layer-info idx]
    (define-values (mask i0) (values (parse-uint8 layer-info idx) (fx+ idx 1)))
    (define-values (udensity i1) (if (bitwise-bit-set? mask 0) (values (parse-uint8 layer-info i0) (fx+ i0 1)) (values #false i0)))
    (define-values (ufeather i2) (if (bitwise-bit-set? mask 1) (values (parse-double layer-info i1) (fx+ i1 8)) (values #false i1)))
    (define-values (vdensity i3) (if (bitwise-bit-set? mask 2) (values (parse-uint8 layer-info i2) (fx+ i2 1)) (values #false i2)))
    (define-values (vfeather i4) (if (bitwise-bit-set? mask 3) (values (parse-double layer-info i3) (fx+ i3 8)) (values #false i3)))
    (values (vector udensity ufeather vdensity vfeather) i4)))

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

(define parse-8B64 : (-> Bytes Fixnum Byte (Values Symbol PSD-Layer-Segment Fixnum))
  (lambda [layer-info start psd/psb-size]
    (define key : Symbol (parse-keyword layer-info (fx+ start 4)))
    (define ssize : Byte
      (if (and (fx> psd/psb-size 4)
               (memq key '(lmsk lr16 lr32 layr mt16 mt32 mtrn alph fmsk lnk2 feid fxid pxsd)))
          psd/psb-size 4))
    (define size : Index (parse-size layer-info (fx+ start 8) ssize))
    (define rstart : Fixnum (fx+ (fx+ start 8) ssize))
    (values key (vector (make-special-comment layer-info) rstart size) (fx+ rstart size))))

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
