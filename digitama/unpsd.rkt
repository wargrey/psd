#lang racket

;;; http://www.adobe.com/devnet-apps/photoshop/fileformatashtml

(provide psd%)

(require racket/draw)
(require racket/fixnum)
(require racket/flonum)
(require images/flomap)

(require "packbits.rkt")

(define {bytes->int bs [signed? #true]}
  (integer-bytes->integer bs signed? #true))

(define {read-int bsize [in (current-input-port)]}
  (define bs (read-bytes bsize in))
  (case bsize
    [{1} (bytes-ref bs 0)]
    [{2 4 8} (bytes->int bs)]
    [else bs]))

(define {read-unicode-char [in (current-input-port)]}
  (integer->char (bytes->int (read-bytes 2 in) #false)))

(define {read-double [in (current-input-port)]}
  (floating-point-bytes->real (read-bytes 8 in) #true))

(define {read-ints bsizes [in (current-input-port)]}
  (map {lambda [size] (cond [(list? size) (read-ints size in)]
                            [else (read-int size in)])}
       bsizes))

(define {read-bytes-seq bsizes [in (current-input-port)]}
  (map (curryr read-bytes in) bsizes))

(define {read-8BIMs smart-size [in (current-input-port)]}
  (if (regexp-match-peek #px#"^8B(IM|64)" in)
      (cons (let ([skip-8BIM (read-bytes 4)])
              (cons (read-bytes 4)
                    (let ([len (read-int smart-size)])
                      (read-bytes len))))
            (read-8BIMs smart-size in))
      null))

(define {bit-at? flg bp}
  (let ([b (arithmetic-shift 1 bp)])
    (= (bitwise-and flg b) b)))                  

(define psd-layer%
  {class object% (super-new)
    (init-field smart-size smart-byte)
    
    (field [top (read-int 4)] [left (read-int 4)] [bottom (read-int 4)] [right (read-int 4)]
           [channels (build-list (read-int 2) {lambda [index] (cons (read-int 2) (read-int smart-size))})]
           [blend (let ([skip-8BIM (read-bytes 4)]) (read-bytes 4))]
           [opacity (read-byte)] [clipping (read-byte)] [flags (read-byte)] [filler (read-byte)])
    
    (define extra-size (read-int 4))
    
    (field [mask (let ([size (read-int 4)])
                   (cond [(zero? size) (make-hash)]
                         [else (make-hash (let* ([ints (read-ints '{4 4 4 4 1})]
                                                 [flag (read-byte)]
                                                 [mpflag (when (bit-at? flag 4) (read-byte))])
                                            (filter-not (compose void? cdr)
                                                        (list* (cons 'top (first ints))
                                                               (cons 'left (second ints))
                                                               (cons 'bottom (third ints))
                                                               (cons 'right (fourth ints))
                                                               (cons 'default-color (fifth ints))
                                                               (cons 'flag flag)
                                                               (cons 'parameter (unless (void? mpflag)
                                                                                       (cond [(bit-at? mpflag 0) (cons mpflag (read-byte))]
                                                                                             [(bit-at? mpflag 1) (cons mpflag (read-double))]
                                                                                             [(bit-at? mpflag 2) (cons mpflag (read-byte))]
                                                                                             [(bit-at? mpflag 3) (cons mpflag (read-double))])))
                                                               (if (= size 20) (let ([padding (read-bytes 2)]) null)
                                                                   (list (cons 'real-flag (read-byte))
                                                                         (cons 'real-default-color (read-byte))
                                                                         (cons 'masked-top (read-int 4))
                                                                         (cons 'masked-left (read-int 4))
                                                                         (cons 'masked-bottom (read-int 4))
                                                                         (cons 'masked-right (read-int 4))))))))]))]
           [blending (let ([len (read-int 4)])
                       (with-input-from-bytes (read-bytes len)
                         {thunk (build-list (/ len 4 2) {lambda [index] (read-ints '{{1 1 1 1} {1 1 1 1}})})}))])
    
    (define padded-name (first (regexp-match #px#"(.(?!8BIM|8B64))+." (current-input-port))))
    
    (field [additional-info (make-hash (read-8BIMs smart-size))])
    
    (define/public {get-name}
      (define datum (hash-ref additional-info #"luni"))
      (when (bytes? datum)
        (set! datum (with-input-from-bytes datum
                      {thunk (string-trim (build-string (/ (bytes-length datum) 2)
                                                        {lambda [whocares] (read-unicode-char)})
                                          #px"[[:cntrl:]]+")}))
        (hash-set! additional-info #"luni" datum))
      datum)
    
    (define/public {get-type}
      (define datum (hash-ref additional-info #"lsct" 0))
      (when (bytes? datum)
        (define len (bytes-length datum))
        (with-input-from-bytes datum
          {thunk (set! datum (make-hash (list (cons 'type (read-int 4)))))
                 (when (>= len 12)
                   (define skip-8BIM (read-bytes 4))
                   (hash-set! datum 'blend (read-bytes 4))
                   (when (>= len 16)
                     (hash-set! datum 'subtype (read-int 4))))
                 (hash-set! additional-info #"lsct" datum)}))
      (if (hash? datum) (hash-ref datum 'type) datum))
    
    (define/public {folder?}
      (member (get-type) '{1 2}))
    
    (define/public {boundary?}
      (= (get-type) 3))
    
    (define/public {get-rectangle}
      (values top left bottom right))
    
    (define/public {get-image}
      (define channel# (length channels))
      (define rgbs (make-hash (map {lambda [metainfo]
                                     (let ([compression (hash-ref metainfo 'compression)]
                                           [image-data (hash-ref metainfo 'image-data)])
                                       (cons (hash-ref metainfo 'id)
                                             (case compression
                                               [{0} image-data]
                                               [{1} (with-input-from-bytes image-data
                                                      {thunk (define rows (build-list (- bottom top) {lambda [whocares] (read-int smart-byte)}))
                                                             (apply bytes-append (map unpackbits (read-bytes-seq rows)))})]
                                               [else (error 'psd% "Unimplemented compression method: ~a" compression)])))}
                                   channels)))
      (define component# (let ([rgb? (curryr member (hash-keys rgbs))])
                           (cond [(andmap rgb? '{-1 0 1 2}) 4]
                                 [(andmap rgb? '{0 1 2}) 3]
                                 [else (error 'psd% "Inproper channels: ~a" (hash-keys rgbs))])))
      (define total (* (- bottom top) (- right left)))
      (define pixels (make-flvector (* total component#)))
      (for ([nth (in-range total)])
        (case component#
          [{4} (let ([alpha (fl/ (fx->fl (bytes-ref (hash-ref rgbs -1) nth)) 255.0)]
                     [pos (* nth 4)])
                 (flvector-set! pixels (+ pos 0) alpha)
                 (for ([off (in-range 3)]) (flvector-set! pixels (+ pos off 1) (fl* alpha (fl/ (fx->fl (bytes-ref (hash-ref rgbs off) nth)) 255.0)))))]
          [{3} (let ([pos (* nth 3)])
                 (for ([off (in-range 3)]) (flvector-set! pixels (+ pos off) (fl/ (fx->fl (bytes-ref (hash-ref rgbs off) nth)) 255.0))))]))
      (flomap pixels component# (- right left) (- bottom top)))

    (define/public {desc [prefix ""] [out (current-output-port)]}
      (define frmt (foldr string-append ""
                          (add-between (list (string-append prefix "Layer Object: ~a") "Rectangle: (~a, ~a, ~a, ~a)" "Channels: ~a"
                                             "Layer Type: ~a" "Blend: ~a" "Opacity: ~a" "Clipping: ~a" "Flags: #b~b" "Mask/Adjustment: ~a"
                                             "Additional Info: ~a~a~n")
                                       (string-append "~n" prefix "    "))))
      (fprintf out frmt (get-name) top left bottom right (length channels)
               (list-ref '{"Normal" "Open Folder" "Closed Folder" "Folder Boundary"} (get-type))
               blend opacity clipping flags mask
               (hash-count additional-info) (hash-keys additional-info)))})

(define psd%
  {class object% (super-new)
    (init path)
    
    (define-values
      {file-header ~version ~channels ~height ~width ~depth ~color-mode ~compression psd/psb-size psd/psb-byte
                   color-mode-data resource-data layer-mask-data image-data}
      (with-input-from-file path
        {thunk (define header (read-bytes-seq '{4 2 6 2 4 4 2 2}))
               (unless (and (equal? (first header) #"8BPS")
                            (member (bytes->int (second header)) '{1 2}))
                 (raise-user-error 'with-input-from-file "this is not a valid PSD/PSB file: ~a" path))
               
               (define meta (map bytes->int (filter {lambda [bs] (member (bytes-length bs) '{2 4})} (cdr header))))
               (define-values {smart-size smart-byte}
                 (case (car meta)
                   [{1} (values 4 2)]
                   [{2} (values 8 4)]))
               
               (define color-mode-size (read-int 4))
               (define color-data (read-bytes color-mode-size))
               
               (define resource-size (read-int 4))
               (define resource (read-bytes resource-size))
               
               (define layer-size (read-int smart-size))
               (define layer (read-bytes layer-size))
               
               (define image-compression (read-int 2))
               (define image-data (port->bytes))
               
               (apply values (foldr bytes-append #"" header) (append meta (list  image-compression smart-size smart-byte
                                                                                 color-data resource layer image-data)))}))
    
    (define-values {image-resources}
      (with-input-from-bytes resource-data
        {thunk (define read-record
                 {lambda [res]
                   (let ([skip-8BIM (read-bytes 4 res)]
                         [id (read-int 2 res)])
                     (cons id (mcons (second (regexp-match #px#"((?:[^\0][^\0])*)\0\0" res))
                                    (let ([len (read-int 4 res)])
                                      (read-bytes len res)))))})
               (make-hash (map (curryr call-with-input-bytes read-record) (regexp-match* #px#"8BIM(.(?!8BIM))+." (current-input-port))))}))
    
    (define-values
      {layers global-mask tagged-blocks}
      (with-input-from-bytes layer-mask-data
        {thunk (values (let ([info-size (read-int psd/psb-size)])
                         (with-input-from-bytes (read-bytes info-size)
                           {thunk (define count (read-int 2))
                                  (vector-map! {lambda [psd-layer]
                                                 (define-values {top left bottom right} (send psd-layer get-rectangle))
                                                 (set-field! channels psd-layer
                                                             (map {lambda [metainfo]
                                                                    (let ([data-size (- (cdr metainfo) 2)])
                                                                      (make-hash (list (cons 'id (car metainfo))
                                                                                       (cons 'compression (read-int 2))
                                                                                       (cons 'image-data (read-bytes data-size)))))}
                                                                  (get-field channels psd-layer)))
                                                 psd-layer}
                                               (build-vector (abs count)
                                                             {lambda [whocares] (make-object psd-layer% psd/psb-size psd/psb-byte)}))}))
                       (let ([mask-size (read-int 4)])
                         (cond [(zero? mask-size) (make-hash)]
                               [else (let ([section (read-ints `{2 {2 2 2 2} 2 1 ,(- mask-size 2 8 2 1)})])
                                       (make-hash (list (cons 'overlay-color-space (first section))
                                                        (cons 'color-components (second section))
                                                        (cons 'opacity (third section))
                                                        (cons 'kind (fourth section)))))]))
                       (make-hash (read-8BIMs psd/psb-size)))}))
    
    (define-values {layer-images} (vector-map {lambda [psd-layer] (mcons (send psd-layer get-name) (void))} layers))
    
    (define/public {get-xmp}
      (mcdr (hash-ref image-resources #x424)))
    
    (define/public {get-grid-guides}
      (define d1032 (hash-ref image-resources #x408))
      (when (bytes? (mcdr d1032))
        (set-mcdr! d1032 (with-input-from-bytes (mcdr d1032)
                           {thunk (make-hash (list (cons 'version (read-int 4))
                                                   (cons 'horizotal (read-int 4))
                                                   (cons 'vertical (read-int 4))
                                                   (cons 'fguides (build-list (read-int 4)
                                                                              {lambda [whocares] (read-ints '{4 1})}))))})))
      (mcdr d1032))
    
    (define/public {get-thumbnail}
      (define d1036 (hash-ref image-resources #x40C))
      (when (bytes? (mcdr d1036))
        (set-mcdr! d1036 (make-object bitmap% (open-input-bytes (subbytes (mcdr d1036) 28)))))
      (mcdr d1036))
    
    (define/public {get-images-by-layers layer-names}
      (define names (if (list? layer-names) (map ~a layer-names) (list (~a layer-names))))
      (filter-not void?
                  (for/list ([psd-layer (in-vector layers)]
                             [layer-image (in-vector layer-images)])
                    (when (and (false? (send psd-layer folder?))
                               (false? (send psd-layer boundary?))
                               (member (send psd-layer get-name) names))
                      (when (void? (mcdr layer-image))
                        (set-mcdr! layer-image (send psd-layer get-image)))
                      (mcdr layer-image)))))})
