#lang typed/racket/base

;;; http://www.adobe.com/devnet-apps/photoshop/fileformatashtml

(provide (all-defined-out))
(provide (rename-out [bytes-ref parse-uint8]))

(require racket/fixnum)

(define parse-pascal-string : (-> Bytes Integer (Values String Byte))
  (lambda [src start]
    (define size : Byte (bytes-ref src start))
    (define bstart : Integer (fx+ start 1))
    (values (bytes->string/utf-8 src #false bstart (fx+ bstart size))
            size)))

(define parse-unicode-string : (-> Bytes Integer (Values String Index))
  (lambda [src start]
    (define size : Index (parse-uint32 src start index?))
    (cond [(fx= size 0) (values "" 0)]
          [else (let ([buffer (make-string size #\null)]
                      [max-idx (fx- size 1)])
                  (let fill-string! ([src-idx : Integer (fx+ start 4)]
                                     [dest-idx : Integer 0])
                    (string-set! buffer dest-idx (parse-char src src-idx))
                    (if (fx< dest-idx max-idx)
                        (fill-string! (fx+ src-idx 2) (fx+ dest-idx 1))
                        (values buffer (assert (fx* size 2) index?)))))])))

(define parse-char : (-> Bytes Integer Char)
  (lambda [src start]
    (integer->char (parse-uint16 src start))))

(define parse-int16 : (All (a) (case-> [Bytes Integer -> Integer]
                                       [Bytes Integer (-> Any Boolean : a) -> a]))
  (case-lambda
    [(src start) (integer-bytes->integer src #true #true start (fx+ start 2))]
    [(src start subinteger?) (assert (parse-int16 src start) subinteger?)]))

(define parse-uint16 : (All (a) (case-> [Bytes Integer -> Index]
                                        [Bytes Integer (-> Any Boolean : a) -> a]))
  (case-lambda
    [(src start) (parse-size src start 2)]
    [(src start subinteger?) (parse-size src start 2 subinteger?)]))

(define parse-int32 : (All (a) (case-> [Bytes Integer -> Integer]
                                         [Bytes Integer (-> Any Boolean : a) -> a]))
  (case-lambda
    [(src start) (integer-bytes->integer src #true #true start (fx+ start 4))]
    [(src start subinteger?) (assert (parse-int32 src start) subinteger?)]))

(define parse-uint32 : (All (a) (case-> [Bytes Integer -> Natural]
                                          [Bytes Integer (-> Any Boolean : a) -> a]))
  (case-lambda
    [(src start) (parse-size src start 4)]
    [(src start subinteger?) (parse-size src start 4 subinteger?)]))

(define parse-int64 : (All (a) (case-> [Bytes Integer -> Integer]
                                      [Bytes Integer (-> Any Boolean : a) -> a]))
  (case-lambda
    [(src start) (integer-bytes->integer src #true #true start (fx+ start 8))]
    [(src start subinteger?) (assert (parse-int64 src start) subinteger?)]))

(define parse-size : (All (a) (case-> [Bytes Integer Integer -> Index]
                                        [Bytes Integer Integer (-> Any Boolean : a) -> a]))
  (case-lambda
    [(src start size) (parse-size src start size index?)]
    [(src start size size?) (assert (integer-bytes->integer src #false #true start (fx+ start size)) size?)]))

(define parse-nsizes-list : (-> Bytes Integer Integer Integer (Listof Index))
  (lambda [src start size count]
    (let parse ([idx : Fixnum (fx+ (fx* (fx- count 1) size) start)]
                [dest : (Listof Index) null])
      (cond [(fx< idx start) dest]
            [else (let ([n (parse-size src idx size)])
                    (parse (fx- idx size) (cons n dest)))]))))

(define parse-double : (-> Bytes Integer Flonum)
  (lambda [src start]
    (with-asserts ([start index?])
      (floating-point-bytes->real src #true start (fx+ start 8)))))

(define parse-nbytes : (-> Bytes Integer Integer Bytes)
  (lambda [src start bsize]
    (subbytes src start (fx+ start bsize))))

(define parse-nbytes-list : (-> Bytes Integer (Listof Index) (Listof Bytes))
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

(define bit-at? : (-> Integer Integer Boolean)
  (lambda [flg bp]
    (let ([b (arithmetic-shift 1 bp)])
      (= (bitwise-and flg b) b))))

(define nbytes-pairs : (-> Integer (Listof Index) (Listof (Pairof Integer Integer)))
  (lambda [start bsizes]
    (let parse ([last-end : Integer start]
                [sizes : (Listof Index) bsizes]
                [dest : (Listof (Pairof Integer Integer)) null])
      (cond [(null? sizes) (reverse dest)]
            [else (let ([next-end (fx+ last-end (car sizes))])
                    (parse next-end (cdr sizes) (cons (cons last-end next-end) dest)))]))))
