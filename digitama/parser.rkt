#lang typed/racket/base

;;; http://www.adobe.com/devnet-apps/photoshop/fileformatashtml

(provide (all-defined-out))
(provide (all-from-out "integer.rkt"))
(provide (rename-out [bytes-ref parse-uint8]))

(require "integer.rkt")

(define parse-boolean : (-> Bytes Fixnum Boolean)
  (lambda [src start]
    (fx> (bytes-ref src start) 0)))

(define parse-pascal-string : (-> Bytes Fixnum (Values String Fixnum))
  (lambda [src start]
    (define-values (pascal _size pend) (pascal-string src start))
    (values pascal pend)))

(define parse-pascal-string*n : (-> Bytes Fixnum Byte (Values String Fixnum))
  (lambda [src start n]
    (define-values (pascal psize pend) (pascal-string src start))
    (values pascal (fx+ pend (fx- (fx- n 1) (fxremainder psize n))))))

(define parse-unicode-string : (-> Bytes Fixnum (Values String Fixnum))
  (lambda [src start]
    (define size : Index (parse-size src start 4))
    (cond [(fx= size 0) (values "" size)]
          [else (let-values ([(unicode chwidth) (values (make-string size #\null) 2)])
                  (let fill-string! ([dest-idx : Fixnum 0] [src-idx : Fixnum (fx+ start 4)])
                    (when (fx< dest-idx size)
                      (define next-idx : Fixnum (fx+ src-idx chwidth))
                      (string-set! unicode dest-idx (integer->char (integer-bytes->integer src #false #true src-idx next-idx)))
                      (fill-string! (fx+ dest-idx 1) next-idx)))
                  (values unicode (fx+ (fx+ start 4) (fx* size chwidth))))])))

(define parse-keyword : (All (a) (case-> [Bytes Fixnum -> Symbol]
                                         [Bytes Fixnum (-> Any Boolean : #:+ a) -> a]))
  (case-lambda
    [(src start key?) (assert (parse-keyword src start) key?)]
    [(src start)
     (let ([3chkey? : Boolean (fx= (bytes-ref src (fx+ start 3)) 32)])
       (define key : String (bytes->string/utf-8 src #false start (fx+ start (if 3chkey? 3 4))))
       (string->symbol key))]))

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
(define nbytes-pairs : (-> Fixnum (Listof Index) (Listof (Pairof Integer Integer)))
  (lambda [start bsizes]
    (let parse ([last-end : Fixnum start]
                [sizes : (Listof Index) bsizes]
                [dest : (Listof (Pairof Integer Integer)) null])
      (cond [(null? sizes) (reverse dest)]
            [else (let ([next-end (fx+ last-end (car sizes))])
                    (parse next-end (cdr sizes) (cons (cons last-end next-end) dest)))]))))

(define pascal-string : (-> Bytes Fixnum (Values String Fixnum Fixnum))
  (lambda [src start]
    ;;; NOTE: Pascal strings are usually useless if they contains not-ASCII code
    ;;;   in which case there should be Unicode strings stored elsewhere.
    (define size : Byte (bytes-ref src start))
    (define bstart : Fixnum (fx+ start 1))
    (define bend : Fixnum (fx+ bstart size))
    (values (bytes->string/latin-1 src #false bstart bend) size bend)))
