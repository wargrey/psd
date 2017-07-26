#lang typed/racket/base

(provide (all-defined-out) unsafe-provide)

(require typed/racket/unsafe)

(define-type PSD-Layer-Segment (Vector Special-Comment Fixnum Index))
(define-type PSD-Layer-Infobase (HashTable Symbol (U PSD-Layer-Segment PSD-Layer-Info)))
(define-type PSD-Layer-Info-Parser (-> Bytes Fixnum Index (Listof Any) PSD-Layer-Info))

(struct PSD-Layer-Info () #:transparent)

(struct PSD-Layer-Id PSD-Layer-Info
  ([id : Natural])
  #:transparent)

(struct PSD-Layer-Unicode-Name PSD-Layer-Info
  ([data : String])
  #:transparent)

(struct PSD-Layer-Text-Engine PSD-Layer-Info
  ([data : Special-Comment])
  #:transparent)

(struct PSD-Layer-Section-Divider PSD-Layer-Info
  ([type : Integer]
   [blend-mode : (Option Symbol)]
   [subtype : (Option Integer)])
  #:transparent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define /psd/layer/blocks : Path (collection-file-path "blocks" "psd" "digitama" "layer"))
