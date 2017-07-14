#lang typed/racket/base

(provide 0x408)

(require racket/string)

(require "format.rkt")
(require "../parser.rkt")
(require "../draw.rkt")

(define 0x408 : (-> Integer Bytes String Null PSD-Grid+Guides)
  (lambda [id block name argl]
    (psd-grid+guides id name
                     (parse-uint32 block 0)
                     (parse-uint32 block 4)
                     (parse-uint32 block 8)
                     (let parse-guide ([fgridcount : Index (parse-size block 12 4)]
                                       [start : Integer 16]
                                       [guides : (Listof (Pairof Integer PSD-Guide-Direction)) null])
                       (cond [(fx= fgridcount 0) (reverse guides)]
                             [else (parse-guide (fx- fgridcount 1)
                                                (fx+ start 5)
                                                (cons (cons (parse-int32 block start)
                                                            (integer->vhselect (parse-uint8 block (fx+ start 4))))
                                                      guides))])))))
