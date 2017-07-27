#lang typed/racket/base

(provide (all-defined-out))

(require racket/vector)

(require "base.rkt")
(require "layer.rkt")

(require "digitama/psd.rkt")
(require "digitama/resource.rkt")
(require "digitama/layer.rkt")

(define psd-profile : (->* (PSD) (Output-Port #:prefix String #:full? Boolean) Void)
  (lambda [self [out (current-output-port)] #:prefix [prefix ""] #:full? [full? #true]]
    (define-values (~t ~t~t) (values (string-append prefix "    ") (string-append prefix "        ")))

    (fprintf out "~a~a Object[~a]:~n" prefix (if (PSD? self) 'PSD 'PSB) (PSD-File-name self))
    (fprintf out "~aSize: [~a * ~a]~n" ~t (PSD-Header-width self) (PSD-Header-height self))
    (fprintf out "~aDepth: [~a * ~a]~n" ~t (PSD-Header-depth self) (PSD-Header-channels self))
    (fprintf out "~aColor Mode: ~a~n" ~t (PSD-Header-color-mode self))
    (fprintf out "~aCompression Method: ~a~n" ~t (PSD-Header-compression-method self))

    (define resources : PSD-Image-Resources (psd-image-resources self #:resolve? #false))
    (define layers : (Listof PSD-Layer-Object) (psd-layers self))
    (define mask : (Option PSD-Global-Layer-Mask) (psd-global-layer-mask self))
    (define infobase : PSD-Layer-Infobase (psd-tagged-blocks self #:resolve? #false))
    
    (fprintf out "~aResources: ~a~n" ~t (hash-keys resources))
    (fprintf out "~aLayer Count: ~a~n" ~t (length layers))
    (fprintf out "~aGlobal Mask: ~a~n" ~t (if (not mask) 'None (vector-drop (struct->vector mask) 2)))
    (fprintf out "~aTagged Blocks: ~a~n~n" ~t (hash-keys infobase))
    
    (unless (not full?)
      (for ([layer (in-list layers)])
        (psd-layer-profile layer out #:prefix ~t)))))

(define psd-layer-profile : (->* (PSD-Layer-Object) (Output-Port #:prefix String) Void)
  (lambda [self [out (current-output-port)] #:prefix [prefix ""]]
    (define-values (~t ~t~t) (values (string-append prefix "    ") (string-append prefix "        ")))

    (define +/- : Symbol (if (PSD-Layer-Header-has-transparency-data? self) '- '+))
    (define record : PSD-Layer-Record (PSD-Layer-Object-record self))
    (define mask : (Option PSD-Layer-Mask) (PSD-Layer-Record-mask record))
    (fprintf out "~aLayer Object[~a]: '~a'~n" prefix (PSD-Layer-Header-id self) (PSD-Layer-Header-name self))
    (fprintf out "~aType: ~a~n" ~t (cond [(PSD-Layer:Open? self) "Open Folder"]
                                         [(PSD-Layer:Closed? self) "Closed Folder"]
                                         [(PSD-Layer:Divider? self) "Folder Boundary"]
                                         [else "Normal"]))
    
    (fprintf out "~aLocation: (~a, ~a)~n" ~t (PSD-Layer-Record-x record) (PSD-Layer-Record-y record))
    (fprintf out "~aSize: [~a * ~a]~n" ~t (PSD-Layer-Record-width record) (PSD-Layer-Record-height record))
    (fprintf out "~aChannels: ~a~a~n" ~t +/- (for/list ([ch (in-list (PSD-Layer-Header-channels self))]) (cons (car ch) (cadr ch))))
    (fprintf out "~aBlend Mode: ~a~n" ~t (PSD-Layer-Record-blend record))
    (fprintf out "~aOpacity: ~a~n" ~t (PSD-Layer-Record-opacity record))
    (fprintf out "~aClipping: ~a~n" ~t (if (PSD-Layer-Record-base-clipping? record) 'base 'nonbase))
    (fprintf out "~aFlags: ~a~n" ~t (PSD-Layer-Record-flags record))
    (fprintf out "~aMask: ~a~n" ~t (or mask 'None))
    (fprintf out "~aAdditional Information: ~a~n~n" ~t (hash-keys (PSD-Layer-Object-infobase self)))))
