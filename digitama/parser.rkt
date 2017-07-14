#lang typed/racket/base

;;; http://www.adobe.com/devnet-apps/photoshop/fileformatashtml

(provide (all-defined-out))

(require racket/fixnum)

(define parse-pascal-string : (-> Bytes Integer (Values String Byte))
  (lambda [src start]
    (define size : Byte (bytes-ref src start))
    (define bstart : Integer (fx+ start 1))
    (values (bytes->string/utf-8 src #false bstart (fx+ bstart size))
            size)))

(define parse-integer : (All (a) (case-> [Bytes Integer Boolean Integer -> Integer]
                                         [Bytes Integer Boolean (-> Any Boolean : a) Integer -> a]))
  (case-lambda
    [(src size signed? start)
     (define end : Fixnum (fx+ start size))
     (integer-bytes->integer src signed? #true start end)]
    [(src size signed? subinteger? start)
     (assert (parse-integer src size signed? start) subinteger?)]))

(define parse-nsizes-list : (-> Bytes Integer Integer Integer (Listof Index))
  (lambda [src count size start]
    (let parse ([idx : Fixnum (fx+ (fx* (fx- count 1) size) start)]
                [dest : (Listof Index) null])
      (cond [(not (index? idx)) dest]
            [else (let ([n (parse-integer src size #false index? idx)])
                    (parse (fx- idx size) (cons n dest)))]))))

(define parse-double-flonum : (-> Bytes Integer Flonum)
  (lambda [src start]
    (with-asserts ([start index?])
      (define end : Positive-Fixnum (fx+ start 8))
      (floating-point-bytes->real src #true start end))))

(define parse-nbytes : (-> Bytes Integer Integer Bytes)
  (lambda [src bsize start]
    (subbytes src start (fx+ start bsize))))

(define parse-nbytes-list : (-> Bytes (Listof Index) Integer (Listof Bytes))
  (lambda [src bsizes start]
    (for/list : (Listof Bytes) ([interval (in-list (nbytes-pairs bsizes start))])
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

(define nbytes-pairs : (-> (Listof Index) Integer (Listof (Pairof Integer Integer)))
  (lambda [bsizes start]
    (let parse ([last-end : Integer start]
                [sizes : (Listof Index) bsizes]
                [dest : (Listof (Pairof Integer Integer)) null])
      (cond [(null? sizes) (reverse dest)]
            [else (let ([next-end (fx+ last-end (car sizes))])
                    (parse next-end (cdr sizes) (cons (cons last-end next-end) dest)))]))))
