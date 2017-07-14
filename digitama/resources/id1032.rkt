#lang typed/racket/base

(provide 0x408)

(require racket/string)

(require "format.rkt")
(require "../parser.rkt")
(require "../draw.rkt")

(define 0x408 : (-> Integer Bytes String Null PSD-Grid+Guides)
  (lambda [id block name argl]
    (psd-grid+guides id name
                     (parse-integer block 4 #false 0)
                     (parse-integer block 4 #false 4)
                     (parse-integer block 4 #false 8)
                     (let parse-guide ([fgridcount : Index (parse-integer block 4 #false index? 12)]
                                       [start : Integer 16]
                                       [guides : (Listof (Pairof Integer PSD-Guide-Direction)) null])
                       (cond [(fx= fgridcount 0) (reverse guides)]
                             [else (parse-guide (fx- fgridcount 1)
                                                (fx+ start 5)
                                                (cons (cons (parse-integer block 4 #false start)
                                                            (integer->vhselect (parse-integer block (fx+ start 4))))
                                                      guides))])))))
