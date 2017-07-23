#lang typed/racket/base

(provide 0x408)

(require racket/string)

(require "format.rkt")
(require "../parser.rkt")
(require "../draw.rkt")

(define 0x408 : (-> Integer String Bytes Fixnum Index Null PSD-Grid+Guides)
  (lambda [id name block idx size argl]
    (PSD-Grid+Guides id name
                     (parse-uint32 block idx) ; version
                     (parse-uint32 block (fx+ idx 4)) ; horizontal
                     (parse-uint32 block (fx+ idx 8)) ; vertical
                     (let parse-guide ([fgridcount : Index (parse-size block (fx+ idx 12) 4)]
                                       [start : Fixnum (fx+ idx 16)]
                                       [guides : (Listof (Pairof Fixnum PSD-Guide-Direction)) null])
                       (cond [(fx= fgridcount 0) (reverse guides)]
                             [else (parse-guide (fx- fgridcount 1)
                                                (fx+ start 5)
                                                (cons (cons (parse-int32 block start) ; location
                                                            (integer->vhselect (parse-uint8 block (fx+ start 4))))
                                                      guides))])))))
