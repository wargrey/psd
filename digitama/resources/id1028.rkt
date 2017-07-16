#lang typed/racket/base

(provide 0x404)

;;; Documentation/IPTC/iimv4.pdf

(require racket/fixnum)

(require "format.rkt")
(require "../parser.rkt")
(require "../exn.rkt")

(define 0x404 : (-> Integer Bytes String Null PSD-File-Info)
  (lambda [id iptc-naa name argl]
    (define total : Index (bytes-length iptc-naa))
    (let parse ([start : Integer 0]
                [entries : (Listof (Pairof Complex Bytes)) null])
      (cond [(and (fx< start total) (eq? (bytes-ref iptc-naa start) #x1C #|ASCII Code of File Separator|#))
             (define-values (record: dataset data-size data) (parse-dataset iptc-naa start))
             (parse (fx+ (fx+ start 5) data-size)
                    (cons (cons (make-rectangular record: dataset) data) entries))]
            [else (displayln (reverse entries))]))
    (psd-file-info id name iptc-naa)))

(define parse-dataset : (-> Bytes Integer (Values Byte Byte Natural Bytes))
  (lambda [iptc-naa start]
    (define-values (record-number: dataset-number)
      ; IIM identifies dataset tags with shape `rn:dsn`
      (values (parse-uint8 iptc-naa (fx+ start 1))
              (parse-uint8 iptc-naa (fx+ start 2))))
    (define data-size : Integer (parse-int16 iptc-naa (fx+ start 3) fixnum?))
    (if (fx> data-size 0)
        (values record-number: dataset-number data-size
                (parse-iimv4 record-number: dataset-number iptc-naa (fx+ start 5) data-size))
        (throw-unsupported-error 'parse-dataset
                                 "confronted an extended dataset in resource: ~a"
                                 (psd-id->string 1028)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define parse-iimv4 : (-> Byte Byte Bytes Integer Integer Bytes)
  (lambda [record dataset src start size]
    (case record
      [else (parse-nbytes src start size)])))

;(define parse-iimv4-record1)
