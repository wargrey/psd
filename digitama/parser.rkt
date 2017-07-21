#lang typed/racket/base

;;; http://www.adobe.com/devnet-apps/photoshop/fileformatashtml

(provide (all-defined-out))
(provide (rename-out [bytes-ref parse-uint8]))

(require "integer.rkt")

(define parse-pascal-string : (-> Bytes Fixnum (Values String Byte))
  (lambda [src start]
    (define size : Byte (bytes-ref src start))
    (define bstart : Fixnum (fx+ start 1))
    (values (bytes->string/utf-8 src #false bstart (fx+ bstart size))
            size)))

(define parse-unicode-string : (-> Bytes Fixnum (Values String Index))
  (lambda [src start]
    (define size : Index (parse-uint32 src start index?))
    (cond [(fx= size 0) (values "" 0)]
          [else (let ([buffer (make-string size #\null)]
                      [max-idx (fx- size 1)])
                  (let fill-string! ([src-idx : Fixnum (fx+ start 4)]
                                     [dest-idx : Fixnum 0])
                    (string-set! buffer dest-idx (parse-char src src-idx))
                    (if (fx< dest-idx max-idx)
                        (fill-string! (fx+ src-idx 2) (fx+ dest-idx 1))
                        (values buffer (assert (fx* size 2) index?)))))])))

(define parse-keyword : (All (a) (-> Bytes Fixnum Byte (-> Any Boolean : #:+ a) a))
  (lambda [src start size key?]
    (define key : String (bytes->string/utf-8 src #false start (fx+ start 4)))
    (assert (string->symbol key) key?)))

(define parse-char : (-> Bytes Fixnum Char)
  (lambda [src start]
    (integer->char (parse-uint16 src start))))

(define parse-int16 : (All (a) (case-> [Bytes Fixnum -> Fixnum]
                                       [Bytes Fixnum (-> Any Boolean : a) -> a]))
  (case-lambda
    [(src start) (parse-int16 src start fixnum?)]
    [(src start subinteger?) (assert (integer-bytes->integer src #true #true start (fx+ start 2)) subinteger?)]))

(define parse-uint16 : (All (a) (case-> [Bytes Fixnum -> Index]
                                        [Bytes Fixnum (-> Any Boolean : a) -> a]))
  (case-lambda
    [(src start) (parse-size src start 2)]
    [(src start subinteger?) (parse-size src start 2 subinteger?)]))

(define parse-int32 : (All (a) (case-> [Bytes Fixnum -> Fixnum]
                                       [Bytes Fixnum (-> Any Boolean : a) -> a]))
  (case-lambda
    [(src start) (parse-int32 src start fixnum?)]
    [(src start subinteger?) (assert (integer-bytes->integer src #true #true start (fx+ start 4)) subinteger?)]))

(define parse-uint32 : (All (a) (case-> [Bytes Fixnum -> Nonnegative-Fixnum]
                                        [Bytes Fixnum (-> Any Boolean : a) -> a]))
  (case-lambda
    [(src start) (parse-uint32 src start nonnegative-fixnum?)]
    [(src start subinteger?) (assert (integer-bytes->integer src #false #true start (fx+ start 4)) subinteger?)]))

(define parse-size : (All (a) (case-> [Bytes Fixnum Fixnum -> Index]
                                      [Bytes Fixnum Fixnum (-> Any Boolean : a) -> a]))
  (case-lambda
    [(src start size) (parse-size src start size index?)]
    [(src start size size?) (assert (integer-bytes->integer src #false #true start (fx+ start size)) size?)]))

(define parse-nsizes-list : (-> Bytes Fixnum Fixnum Fixnum (Listof Index))
  (lambda [src start size count]
    (let parse ([idx : Fixnum (fx+ (fx* (fx- count 1) size) start)]
                [dest : (Listof Index) null])
      (cond [(fx< idx start) dest]
            [else (let ([n (parse-size src idx size)])
                    (parse (fx- idx size) (cons n dest)))]))))

(define parse-double : (-> Bytes Fixnum Flonum)
  (lambda [src start]
    (with-asserts ([start index?])
      (floating-point-bytes->real src #true start (fx+ start 8)))))

(define parse-nbytes : (-> Bytes Fixnum Fixnum Bytes)
  (lambda [src start bsize]
    (subbytes src start (fx+ start bsize))))

(define parse-nbytes-list : (-> Bytes Fixnum (Listof Index) (Listof Bytes))
  (lambda [src start bsizes]
    (for/list : (Listof Bytes) ([interval (in-list (nbytes-pairs start bsizes))])
      (subbytes src (car interval) (cdr interval)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#;(define read-8BIMs : (->* (Integer) (Input-Port) (Listof Bytes))
  (lambda [smart-size [/dev/psdin (current-input-port)]]
    (if (regexp-match-peek #px#"^8B(IM|64)" in)
        (cons (let ([skip-8BIM (read-bytes 4)])
                (cons (read-bytes 4)
                      (let ([len (read-integer smart-size)])
                        (read-bytes len))))
              (read-8BIMs smart-size in))
        null)))

(define nbytes-pairs : (-> Fixnum (Listof Index) (Listof (Pairof Integer Integer)))
  (lambda [start bsizes]
    (let parse ([last-end : Fixnum start]
                [sizes : (Listof Index) bsizes]
                [dest : (Listof (Pairof Integer Integer)) null])
      (cond [(null? sizes) (reverse dest)]
            [else (let ([next-end (fx+ last-end (car sizes))])
                    (parse next-end (cdr sizes) (cons (cons last-end next-end) dest)))]))))
