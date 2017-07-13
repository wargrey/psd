#lang typed/racket/base

;;; http://www.adobe.com/devnet-apps/photoshop/fileformatashtml

(provide (all-defined-out))

(require racket/fixnum)

(define positive-byte? : (-> Any Boolean : Positive-Byte) (λ [v] (and (byte? v) (> v 0))))
(define positive-index? : (-> Any Boolean : Positive-Index) (λ [v] (and (index? v) (> v 0))))

(define read-n:bytes : (-> Input-Port Fixnum Bytes)
  (lambda [/dev/psdin size]
    (read-nbytes* /dev/psdin (read-integer /dev/psdin size #false))))

(define read-integer : (All (a) (case-> [Input-Port Fixnum Boolean -> Integer]
                                        [Input-Port Fixnum Boolean (-> Any Boolean : a) -> a]))
  (case-lambda
    [(/dev/psdin bsize signed?) (integer-bytes->integer (read-bytes* /dev/psdin bsize) signed? #true 0 bsize)]
    [(/dev/psdin bsize signed? subinteger?) (assert (read-integer /dev/psdin bsize signed?) subinteger?)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define parse-integer : (All (a) (case-> [Bytes Index Boolean Index -> Integer]
                                         [Bytes Index Boolean (-> Any Boolean : a) Index -> a]))
  (case-lambda
    [(src size signed? start)
     (define end : Nonnegative-Fixnum (fx+ start size))
     (integer-bytes->integer src signed? #true start end)]
    [(src size signed? subinteger? start)
     (assert (parse-integer src size signed? start) subinteger?)]))

(define parse-nsizes-list : (-> Bytes Index Index Index (Listof Index))
  (lambda [src count size start]
    (let parse ([idx : Fixnum (fx+ (fx* (fx- count 1) size) start)]
                [dest : (Listof Index) null])
      (cond [(not (index? idx)) dest]
            [else (let ([n (parse-integer src size #false index? idx)])
                    (parse (fx- idx size) (cons n dest)))]))))

(define parse-double-flonum : (-> Bytes Index Flonum)
  (lambda [src start]
    (define end : Nonnegative-Fixnum (fx+ start 8))
    (floating-point-bytes->real src #true start end)))

(define parse-nbytes : (-> Bytes Index Index Bytes)
  (lambda [src bsize start]
    (subbytes src start (fx+ start bsize))))

(define parse-nbytes-list : (-> Bytes (Listof Index) Index (Listof Bytes))
  (lambda [src bsizes start]
    (for/list : (Listof Bytes) ([interval (in-list (nbytes-pairs bsizes start))])
      (subbytes src (car interval) (cdr interval)))))

#;(define read-8BIMs : (->* (Integer) (Input-Port) (Listof Bytes))
  (lambda [smart-size [/dev/psdin (current-input-port)]]
    (if (regexp-match-peek #px#"^8B(IM|64)" in)
        (cons (let ([skip-8BIM (read-bytes 4)])
                (cons (read-bytes 4)
                      (let ([len (read-integer smart-size)])
                        (read-bytes len))))
              (read-8BIMs smart-size in))
        null)))

(define bit-at? : (-> Integer Integer Boolean)
  (lambda [flg bp]
    (let ([b (arithmetic-shift 1 bp)])
      (= (bitwise-and flg b) b))))

(define nbytes-pairs : (-> (Listof Index) Index (Listof (Pairof Integer Integer)))
  (lambda [bsizes start]
    (let parse ([last-end : Fixnum start]
                [sizes : (Listof Index) bsizes]
                [dest : (Listof (Pairof Integer Integer)) null])
      (cond [(null? sizes) (reverse dest)]
            [else (let ([next-end (fx+ last-end (car sizes))])
                    (parse next-end (cdr sizes) (cons (cons last-end next-end) dest)))]))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define read-bytes* : (-> Input-Port Integer Bytes)
  (lambda [/dev/psdin size]
    (define bs : (U Bytes EOF) (read-bytes size /dev/psdin))
    (if (bytes? bs) bs (throw-eof-error /dev/psdin))))

(define read-nbytes* : (-> Input-Port Integer Bytes)
  (lambda [/dev/psdin size]
    (define bs : (U Bytes EOF) (read-bytes size /dev/psdin))
    (cond [(and (bytes? bs) (= (bytes-length bs) size)) bs]
          [else (throw-eof-error /dev/psdin)])))

(define throw-eof-error : (-> Input-Port Nothing)
  (lambda [/dev/psdin]
    (raise (make-exn:fail:read:eof "unexpected end of file!"
                                   (continuation-marks #false)
                                   null))))
